{-# OPTIONS_GHC -Wno-missing-fields #-}
{-|
Module      : Z.IO.BIO
Description : Buffered IO interface
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides 'BIO' (block IO) type to facilitate writing streaming programs. A 'BIO' node usually:

  * Process input in unit of block(or item).
  * Running in constant spaces, which means the memory usage won't accumulate.
  * Keep some state in IO, which is sealed in 'BIO' closure.

Some examples of such nodes are:

  * Compressor \/ decompressor, e.g. zlib, etc.
  * Codec, e.g. utf8 codec, base64 codec.
  * Ciphers.
  * Packet parsers.

We use @BIO inp out@ type to represent all the objects above, @BIO Void out@ to represent an 'IO' source,
and @BIO inp Void@ to represent an 'IO' sink, which can all be connected with '>|>' to build a larger 'BIO' node.

@
import Z.Data.CBytes    (CBytes)
import Z.IO
import Z.IO.BIO
import Z.IO.BIO.Zlib

base64AndCompressFile :: HasCallStack => CBytes -> CBytes -> IO ()
base64AndCompressFile origin target = do
    base64Enc <- newBase64Encoder
    (_, zlibCompressor) <- newCompress defaultCompressConfig{compressWindowBits = 31}

    withResource (sourceFromFile origin) $ \ src ->
        withResource (sinkToFile target) $ \ sink ->
            runBIO $ src >|> base64Enc >|> zlibCompressor >|> sink

> base64AndCompressFile "./test" "/test.gz"
-- run 'zcat "/test.gz" | base64 -d' will give you original file
@

-}
module Z.IO.BIO (
  -- * The BIO type
    BIO(..), Source, Sink
  , (>|>), (>~>), (>!>), appendSource
  , concatSource, zipSource
  , joinSink, fuseSink
  , ParseException(..)
  -- * Run BIO chain
  , runBIO
  , runSource, runSource_
  , runBlock, runBlock_, unsafeRunBlock
  , runBlocks, runBlocks_, unsafeRunBlocks
  -- * Make new BIO
  , sourceFromList
  , sinkToList
  , sourceFromBuffered
  , sourceTextFromBuffered
  , sinkToBuffered
  , sinkBuilderToBuffered
  -- * Input & Output BIO adapters
  , sourceFromInput
  , sourceFromFile
  , sourceTextFromInput
  , sourceParsedInput
  , sinkToOutput
  , sinkToFile
  , sinkBuilderToOutput
  , sinkToIO
  -- * Bytes specific
  , newParserNode, newReChunk, newUTF8Decoder, newMagicSplitter, newLineSplitter
  , newBase64Encoder, newBase64Decoder
  , hexEncoder, newHexDecoder
  -- * Generic BIO
  , newCounterNode
  , newSeqNumNode
  , newGroupingNode
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import qualified Data.List             as List
import           Data.Void
import           Data.Word
import           Data.Bits             ((.|.))
import           System.IO.Unsafe      (unsafePerformIO)
import qualified Z.Data.Array          as A
import qualified Z.Data.Builder        as B
import qualified Z.Data.Parser         as P
import           Z.Data.CBytes         (CBytes)
import           Z.Data.PrimRef
import qualified Z.Data.Text           as T
import qualified Z.Data.Text.UTF8Codec as T
import qualified Z.Data.Vector         as V
import qualified Z.Data.Vector.Base    as V
import           Z.Data.Vector.Base64
import           Z.Data.Vector.Hex
import           Z.IO.Buffered
import           Z.IO.Exception
import qualified Z.IO.FileSystem       as FS
import           Z.IO.Resource

-- | A 'BIO'(blocked IO) node.
--
-- A 'BIO' node consist of two functions: 'push' and 'pull'. It can be used to describe different kinds of IO
-- devices:
--
--  * @BIO inp out@ describe an IO state machine(e.g. z_stream in zlib),
--    which takes some input in block, then outputs.
--  * @type Source out = BIO Void out@ described an IO source, which never takes input,
--    but gives output until EOF when 'pull'ed.
--  * @type Sink inp = BIO inp Void@ described an IO sink, which takes input and perform some IO effects,
--    such as writing to terminal or files.
--
-- You can connect these 'BIO' nodes with '>|>', which connect left node's output to right node's input,
-- and return a new 'BIO' node with left node's input type and right node's output type.
--
-- You can run a 'BIO' node in different ways:
--
--   * 'runBIO' will continuously pull value from source, push to sink until source reaches EOF.
--   * 'runSource' will continuously pull value from source, and perform effects along the way.
--   * 'runBlock' will supply a single block of input as whole input, and return output if there's any.
--   * 'runBlocks' will supply a list of blocks as whole input, and return a list of output blocks.
--
-- Note 'BIO' usually contains some IO states, you can consider it as an opaque 'IORef':
--
--   * You shouldn't use a 'BIO' node across multiple 'BIO' chain.
--   * You shouldn't use a 'BIO' node across multiple threads.
--
-- Note 'BIO' is just a convenient way to construct single-thread streaming computation, to use 'BIO'
-- in multiple threads, check "Z.IO.BIO.Concurrent" module.
--
data BIO inp out = BIO
    { push :: HasCallStack => inp -> IO (Maybe out)
      -- ^ Push a block of input, perform some effect, and return output,
      -- if input is not enough to produce any output yet, return 'Nothing'.
    , pull :: HasCallStack => IO (Maybe out)
      -- ^ When input reaches EOF, there may be a finalize stage to output trailing output blocks.
      -- return 'Nothing' to indicate current node reaches EOF too.
    }

-- | Type alias for 'BIO' node which never takes input.
--
-- 'push' is not available by type system, and 'pull' return 'Nothing' when reaches EOF.
type Source out = BIO Void out

-- | Type alias for 'BIO' node which only takes input and perform effects.
--
-- 'push' doesn't produce any meaningful output, and 'pull' usually does a flush.
type Sink inp = BIO inp Void

instance Functor (BIO inp) where
    {-# INLINABLE fmap #-}
    fmap f BIO{..} = BIO push_ pull_
      where
        push_ inp = do
            r <- push inp
            return $! fmap f r
        pull_ = do
            r <- pull
            return $! fmap f r

infixl 3 >|>
infixl 3 >~>

-- | Connect two 'BIO' nodes, feed left one's output to right one's input.
(>|>) :: BIO a b -> BIO b c -> BIO a c
{-# INLINE (>|>) #-}
BIO pushA pullA >|> BIO pushB pullB = BIO push_ pull_
  where
    push_ inp = do
        x <- pushA inp
        case x of Just x' -> pushB x'
                  _       -> return Nothing
    pull_ = do
        x <- pullA
        case x of
            Just x' -> do
                y <- pushB x'
                case y of Nothing -> pull_  -- draw input from A until there's an output from B
                          _ -> return y
            _       -> pullB

-- | Flipped 'fmap' for easier chaining.
(>~>) :: BIO a b -> (b -> c) -> BIO a c
{-# INLINE (>~>) #-}
(>~>) = flip fmap

-- | Connect BIO to an effectful function.
(>!>) :: BIO a b -> (b -> IO c) -> BIO a c
{-# INLINE (>!>) #-}
(>!>) BIO{..} f = BIO push_ pull_
  where
    push_ x = push x >>= \ r ->
        case r of Just r' -> Just <$!> f r'
                  _       -> return Nothing
    pull_ = pull >>= \ r ->
        case r of Just r' -> Just <$!> f r'
                  _       -> return Nothing

-- | BIO node from a pure function.
pureBIO :: (a -> b) -> BIO a b
pureBIO f = BIO (\ x -> return (Just (f x))) (return Nothing)

-- | Connect two 'BIO' source, after first reach EOF, draw element from second.
appendSource :: Source a -> Source a  -> IO (Source a)
{-# INLINE appendSource #-}
b1 `appendSource` b2 = concatSource [b1, b2]

-- | Fuse two 'BIO' sink, i.e. everything written to the fused sink will be written to left and right sink.
--
-- Flush result 'BIO' will effectively flush both sink.
joinSink :: Sink out -> Sink out -> Sink out
{-# INLINE joinSink #-}
b1 `joinSink` b2 = fuseSink [b1, b2]

-- | Fuse a list of 'BIO' sinks, everything written to the fused sink will be written to every sink in the list.
--
-- Flush result 'BIO' will effectively flush every sink in the list.
fuseSink :: [Sink out] -> Sink out
{-# INLINABLE fuseSink #-}
fuseSink ss = BIO push_ pull_
  where
    push_ inp = forM_ ss (\ b -> push b inp) >> return Nothing
    pull_ = mapM_ pull ss >> return Nothing

-- | Connect list of 'BIO' sources, after one reach EOF, draw element from next.
concatSource :: [Source a] -> IO (Source a)
{-# INLINABLE concatSource #-}
concatSource ss0 = newIORef ss0 >>= \ ref -> return (BIO{ pull = loop ref})
  where
    loop ref = do
        ss <- readIORef ref
        case ss of
            []       -> return Nothing
            (s:rest) -> do
                r <- pull s
                case r of
                    Just _  -> return r
                    _       -> writeIORef ref rest >> loop ref


-- | Zip two 'BIO' source into one, reach EOF when either one reached EOF.
zipSource :: Source a -> Source b -> Source (a,b)
{-# INLINABLE zipSource #-}
zipSource (BIO _ pullA) (BIO _ pullB) = BIO { pull = do
    mA <- pullA
    mB <- pullB
    return ((,) <$> mA <*> mB)}

-- | Run a 'BIO' loop (source >|> ... >|> sink).
runBIO :: BIO Void Void -> IO ()
{-# INLINABLE runBIO #-}
runBIO BIO{..} = pull >> return ()

-- | Drain a 'BIO' source into a List in memory.
runSource :: Source x -> IO [x]
{-# INLINABLE runSource #-}
runSource BIO{..} = loop pull []
  where
    loop f acc = do
        r <- f
        case r of Just r' -> loop f (r':acc)
                  _       -> return (List.reverse acc)

-- | Drain a source without collecting result.
runSource_ :: Source x -> IO ()
{-# INLINABLE runSource_ #-}
runSource_ BIO{..} = loop pull
  where
    loop f = do
        r <- f
        case r of Just _ -> loop f
                  _       -> return ()

-- | Supply a single block of input, then run BIO node until EOF.
--
-- Note many 'BIO' node will be closed or not be able to take new input after drained.
runBlock :: HasCallStack => BIO inp out -> inp -> IO [out]
{-# INLINABLE runBlock #-}
runBlock BIO{..} inp = do
    x <- push inp
    let acc = case x of Just x' -> [x']
                        _ -> []
    loop pull acc
  where
    loop f acc = do
        r <- f
        case r of Just r' -> loop f (r':acc)
                  _       -> return (List.reverse acc)


-- | Supply a single block of input, then run BIO node until EOF with collecting result.
--
-- Note many 'BIO' node will be closed or not be able to take new input after drained.
runBlock_ :: HasCallStack => BIO inp out -> inp -> IO ()
{-# INLINABLE runBlock_ #-}
runBlock_ BIO{..} inp = do
    _ <- push inp
    loop pull
  where
    loop f = do
        r <- f
        case r of Just _ -> loop f
                  _      -> return ()

-- | Wrap a stream computation into a pure interface.
--
-- You can wrap a stateful BIO computation(including the creation of 'BIO' node),
-- when you can guarantee a computation is pure, e.g. compressing, decoding, etc.
unsafeRunBlock :: HasCallStack => IO (BIO inp out) -> inp -> [out]
{-# INLINABLE unsafeRunBlock #-}
unsafeRunBlock new inp = unsafePerformIO (new >>= \ bio -> runBlock bio inp)

-- | Supply blocks of input, then run BIO node until EOF.
--
-- Note many 'BIO' node will be closed or not be able to take new input after drained.
runBlocks :: HasCallStack => BIO inp out -> [inp] -> IO [out]
{-# INLINABLE runBlocks #-}
runBlocks BIO{..} = loop []
  where
    loop acc (inp:inps) = do
        r <- push inp
        case r of
            Just r' -> loop (r':acc) inps
            _ -> loop acc inps
    loop acc [] = loop' acc
    loop' acc = do
        r <- pull
        case r of
            Just r' -> loop' (r':acc)
            _ -> return (List.reverse acc)

-- | Supply blocks of input, then run BIO node until EOF with collecting result.
--
-- Note many 'BIO' node will be closed or not be able to take new input after drained.
runBlocks_ :: HasCallStack => BIO inp out -> [inp] -> IO ()
{-# INLINABLE runBlocks_ #-}
runBlocks_ bio (inp:inps) = push bio inp >> runBlocks_ bio inps
runBlocks_ bio [] = loop
  where
    loop = do
        r <- pull bio
        case r of
            Just _ -> loop
            _ -> return ()

-- | Wrap a stream computation into a pure interface.
--
-- Similar to 'unsafeRunBlock', but with a list of input blocks.
unsafeRunBlocks :: HasCallStack => IO (BIO inp out) -> [inp] -> [out]
{-# INLINABLE unsafeRunBlocks #-}
unsafeRunBlocks new inps = unsafePerformIO (new >>= \ bio -> runBlocks bio inps)

--------------------------------------------------------------------------------
-- | Turn a 'BufferedOutput' into a 'V.Bytes' sink.
--
sinkToBuffered :: BufferedOutput -> Sink V.Bytes
{-# INLINABLE sinkToBuffered #-}
sinkToBuffered bo = BIO push_ pull_
  where
    push_ inp = writeBuffer bo inp >> pure Nothing
    pull_ = flushBuffer bo >> pure Nothing

-- | Turn a 'BufferedOutput' into a 'B.Builder' sink.
--
sinkBuilderToBuffered :: BufferedOutput -> Sink (B.Builder a)
{-# INLINABLE sinkBuilderToBuffered #-}
sinkBuilderToBuffered bo = BIO push_ pull_
  where
    push_ inp = writeBuilder bo inp >> pure Nothing
    pull_ = flushBuffer bo >> pure Nothing

-- | Turn an 'Output' into 'V,Bytes' sink.
--
-- 'push' will write input to buffer, and 'pull'_ will flush buffer.
sinkToOutput :: HasCallStack => Output o => o -> IO (Sink V.Bytes)
{-# INLINABLE sinkToOutput #-}
sinkToOutput o =
    newBufferedOutput o >>= \ bo -> return (BIO (push_ bo) (pull_ bo))
  where
    push_ bo inp = writeBuffer bo inp >> pure Nothing
    pull_ bo = flushBuffer bo >> pure Nothing

-- | Turn a file into a 'V.Bytes' sink.
--
-- Note the file will be opened in @'FS.O_APPEND' .|. 'FS.O_CREAT' .|. 'FS.O_WRONLY'@ mode,
-- bytes will be written after the end of the original file if there'are old bytes.
sinkToFile :: HasCallStack => CBytes -> Resource (Sink V.Bytes)
{-# INLINABLE sinkToFile #-}
sinkToFile p = do
    f <- FS.initFile p (FS.O_APPEND .|. FS.O_CREAT .|. FS.O_WRONLY) FS.DEFAULT_MODE
    liftIO (sinkToOutput f)

-- | Turn an 'Output' into 'B.Builder' sink.
--
-- 'push' will write input to buffer, and 'pull'_ will flush buffer.
sinkBuilderToOutput :: Output o => o -> IO (Sink (B.Builder ()))
{-# INLINABLE sinkBuilderToOutput #-}
sinkBuilderToOutput o =
    newBufferedOutput o >>= \ bo -> return (BIO (push_ bo) (pull_ bo))
  where
    push_ bo inp = writeBuilder bo inp >> pure Nothing
    pull_ bo = flushBuffer bo >> pure Nothing

-- | Turn an 'Output' into 'BIO' sink.
--
-- 'push' will write input to buffer then perform flush, tend to degrade performance.
sinkToIO :: (a -> IO ()) -> Sink a
{-# INLINABLE sinkToIO #-}
sinkToIO f = BIO push_ pull_
  where
    push_ x = f x >> pure Nothing
    pull_ = pure Nothing

-- | Source a list from memory.
--
sourceFromList :: [a] -> IO (Source a)
sourceFromList xs0 = do
    xsRef <- newIORef xs0
    return BIO{ pull = popper xsRef }
  where
    popper xsRef = do
        xs <- readIORef xsRef
        case xs of
            (x:xs') -> do
                writeIORef xsRef xs'
                return (Just x)
            _ -> return Nothing

-- | Sink to a list in memory.
--
-- The list's 'IORef' is not thread safe here,
-- and list items are in reversed order during sinking(will be reversed when flushed, i.e. pulled),
-- Please don't use it in multiple thread.
--
sinkToList :: IO (IORef [a], Sink a)
sinkToList = do
    xsRef <- newIORef []
    return (xsRef, BIO (\ x -> modifyIORef xsRef (x:) >> return Nothing)
                       (modifyIORef xsRef reverse >> return Nothing))

-- | Turn a 'BufferedInput' into 'BIO' source, map EOF to Nothing.
--
sourceFromBuffered :: BufferedInput -> Source V.Bytes
{-# INLINABLE sourceFromBuffered #-}
sourceFromBuffered i = BIO{ pull = do
    readBuffer i >>= \ x -> if V.null x then return Nothing
                                        else return (Just x)}

-- | Turn a UTF8 encoded 'BufferedInput' into 'BIO' source, map EOF to Nothing.
--
sourceTextFromBuffered :: BufferedInput -> Source T.Text
{-# INLINABLE sourceTextFromBuffered #-}
sourceTextFromBuffered i = BIO{ pull = do
    readBufferText i >>= \ x -> if T.null x then return Nothing
                                            else return (Just x)}

-- | Turn an input device into a 'V.Bytes' source.
sourceFromInput :: Input i => i -> IO (Source V.Bytes)
{-# INLINABLE sourceFromInput #-}
sourceFromInput i =
    newBufferedInput i >>= return . sourceFromBuffered

-- | Turn a file into a 'V.Bytes' source.
sourceFromFile :: CBytes -> Resource (Source V.Bytes)
{-# INLINABLE sourceFromFile #-}
sourceFromFile p = do
    f <- FS.initFile p FS.O_RDONLY FS.DEFAULT_MODE
    liftIO (sourceFromInput f)

-- | Turn an input device into a 'T.Text' source.
sourceTextFromInput :: Input i => i -> IO (Source T.Text)
{-# INLINABLE sourceTextFromInput #-}
sourceTextFromInput i =
    newBufferedInput i >>= return . sourceTextFromBuffered

-- | Turn input device into a packet source, see 'sourceParsedInput''.
sourceParsedInput :: Input i => i -> P.Parser a -> IO (Source a)
{-# INLINABLE sourceParsedInput #-}
sourceParsedInput i p =
    newBufferedInput i >>= return . loopParse
  where
    loopParse bi = BIO{ pull = do
        bs <- readBuffer bi
        if V.null bs
        then return Nothing
        else do
            (rest, r) <- P.parseChunks p (readBuffer bi) bs
            unReadBuffer rest bi
            case r of Right v -> return (Just v)
                      Left e  -> throwIO (ParseException e callStack)}

--------------------------------------------------------------------------------

-- | Make a chunk size divider.
--
-- A divider size divide each chunk's size to the nearest multiplier to granularity,
-- last trailing chunk is directly returned.
newReChunk :: Int                -- ^ chunk granularity
           -> IO (BIO V.Bytes V.Bytes)
{-# INLINABLE newReChunk #-}
newReChunk n = do
    trailingRef <- newIORef V.empty
    return (BIO (push_ trailingRef) (pull_ trailingRef))
  where
    push_ trailingRef bs = do
        trailing <- readIORef trailingRef
        let chunk =  trailing `V.append` bs
            l = V.length chunk
        if l >= n
        then do
            let l' = l - (l `rem` n)
                (chunk', rest) = V.splitAt l' chunk
            writeIORef trailingRef rest
            return (Just chunk')
        else do
            writeIORef trailingRef chunk
            return Nothing
    pull_ trailingRef = do
        trailing <- readIORef trailingRef
        if V.null trailing
        then return Nothing
        else do
            writeIORef trailingRef V.empty
            return (Just trailing)


-- | Exception when parsing failed in streams.
data ParseException = ParseException P.ParseError CallStack deriving Show
instance Exception ParseException where
    toException = ioExceptionToException
    fromException = ioExceptionFromException

-- | Read buffer and parse with 'Parser'.
--
-- This function will continuously draw data from input before parsing finish. Unconsumed
-- bytes will be returned to buffer.
--
-- Return 'Nothing' if reach EOF before parsing, throw 'ParseException' if parsing fail.
newParserNode :: P.Parser a -> IO (BIO V.Bytes a)
{-# INLINABLE newParserNode #-}
newParserNode p = do
    -- type LastParseState = Either V.Bytes (V.Bytes -> P.Result)
    resultRef <- newIORef (Left V.empty)
    return (BIO (push_ resultRef) (pull_ resultRef))
  where
    push_ resultRef bs = do
        lastResult <- readIORef resultRef
        let (chunk, f) = case lastResult of
                Left trailing -> (trailing `V.append` bs, P.parseChunk p)
                Right x -> (bs, x)
        case f chunk of
            P.Success a trailing' -> do
                writeIORef resultRef (Left trailing')
                return (Just a)
            P.Failure e _ ->
                throwIO (ParseException e callStack)
            P.Partial f' -> do
                writeIORef resultRef (Right f')
                return Nothing

    pull_ resultRef = do
        lastResult <- readIORef resultRef
        case lastResult of
            Left trailing ->
                if V.null trailing
                then return Nothing
                else lastChunk resultRef (P.parseChunk p) trailing
            Right f -> lastChunk resultRef f V.empty

    lastChunk resultRef f chunk =
        case f chunk of
            P.Success a trailing' -> do
                writeIORef resultRef (Left trailing')
                return (Just a)
            P.Failure e _ ->
                throwIO (ParseException e callStack)
            P.Partial _ ->
                throwIO (ParseException ["last chunk partial parse"] callStack)

-- | Make a new UTF8 decoder, which decode bytes streams into text streams.
--
--  Note this node is supposed to be used with preprocess node such as compressor, decoder, etc. where bytes
--  boundary cannot be controlled, UTF8 decoder will concat trailing bytes from last block to next one.
--  Use this node directly with 'sourceFromBuffered' \/ 'sourceFromInput' will not be as efficient as directly use
--  'sourceTextFromBuffered' \/ 'sourceTextFromInput', because 'BufferedInput' provides push back capability,
--  trailing bytes can be pushde back to reading buffer and returned with next block input together.
--
newUTF8Decoder :: IO (BIO V.Bytes T.Text)
{-# INLINABLE newUTF8Decoder #-}
newUTF8Decoder = do
    trailingRef <- newIORef V.empty
    return (BIO (push_ trailingRef) (pull_ trailingRef))
  where
    push_ trailingRef bs = do
        trailing <- readIORef trailingRef
        let chunk =  trailing `V.append` bs
            (V.PrimVector arr s l) = chunk
        if l > 0 && T.decodeCharLen arr s <= l
        then do
            let (i, _) = V.findR (\ w -> w >= 0b11000000 || w <= 0b01111111) chunk
            if (i == -1)
            then throwIO (T.InvalidUTF8Exception callStack)
            else do
                if T.decodeCharLen arr (s + i) > l - i
                then do
                    writeIORef trailingRef (V.fromArr arr (s+i) (l-i))
                    return (Just (T.validate (V.fromArr arr s i)))
                else do
                    writeIORef trailingRef V.empty
                    return (Just (T.validate chunk))
        else do
            writeIORef trailingRef chunk
            return Nothing

    pull_ trailingRef = do
        trailing <- readIORef trailingRef
        if V.null trailing
        then return Nothing
        else throwIO (T.InvalidUTF8Exception callStack)

-- | Make a new stream splitter based on magic byte.
--
newMagicSplitter :: Word8 -> IO (BIO V.Bytes V.Bytes)
{-# INLINABLE newMagicSplitter #-}
newMagicSplitter magic = do
    trailingRef <- newIORef V.empty
    return (BIO (push_ trailingRef) (pull_ trailingRef))
  where
    push_ trailingRef bs = do
        trailing <- readIORef trailingRef
        let chunk =  trailing `V.append` bs
        case V.elemIndex magic chunk of
            Just i -> do
                let (line, rest) = V.splitAt (i+1) chunk
                writeIORef trailingRef rest
                return (Just line)
            Nothing -> do
                writeIORef trailingRef chunk
                return Nothing

    pull_ trailingRef = do
        trailing <- readIORef trailingRef
        if V.null trailing
        then return Nothing
        else do
            writeIORef trailingRef V.empty
            return (Just trailing)

-- | Make a new stream splitter based on linefeed(@\r\n@ or @\n@).
--
-- The result bytes doesn't contain linefeed.
newLineSplitter :: IO (BIO V.Bytes V.Bytes)
{-# INLINABLE newLineSplitter #-}
newLineSplitter = do
    s <- newMagicSplitter 10
    return (s >~> dropLineEnd)
  where
    dropLineEnd bs@(V.PrimVector arr s l) =
        case bs `V.indexMaybe` (l-2) of
            Nothing -> V.PrimVector arr s (l-1)
            Just r | r == 13   -> V.PrimVector arr s (l-2)
                   | otherwise -> V.PrimVector arr s (l-1)

-- | Make a new base64 encoder node.
newBase64Encoder :: IO (BIO V.Bytes V.Bytes)
{-# INLINABLE newBase64Encoder #-}
newBase64Encoder = do
    re <- newReChunk 3
    return (re >~> base64Encode)

-- | Make a new base64 decoder node.
newBase64Decoder :: IO (BIO V.Bytes V.Bytes)
{-# INLINABLE newBase64Decoder #-}
newBase64Decoder = do
    re <- newReChunk 4
    return (re >~> base64Decode')

-- | Make a new hex encoder node.
hexEncoder :: Bool   -- ^ uppercase?
           -> BIO V.Bytes V.Bytes
{-# INLINABLE hexEncoder #-}
hexEncoder upper = pureBIO (hexEncode upper)

-- | Make a new hex decoder node.
newHexDecoder :: IO (BIO V.Bytes V.Bytes)
{-# INLINABLE newHexDecoder #-}
newHexDecoder = do
    re <- newReChunk 2
    return (re >~> hexDecode')

-- | Make a new BIO node which counts items flow throught it.
--
-- Returned 'Counter' is increased atomically, it's safe to read \/ reset the counter from other threads.
newCounterNode :: IO (Counter, BIO a a)
{-# INLINABLE newCounterNode #-}
newCounterNode = do
    c <- newCounter 0
    return (c, BIO (push_ c) (return Nothing))
  where
    push_ c x = do
        atomicAddCounter_ c 1
        return (Just x)

-- | Make a new BIO node which counts items, and label item with a sequence number.
--
-- Returned 'Counter' is increased atomically, it's safe to read \/ reset the counter from other threads.
newSeqNumNode :: IO (Counter, BIO a (Int, a))
{-# INLINABLE newSeqNumNode #-}
newSeqNumNode = do
    c <- newCounter 0
    return (c, BIO (push_ c) (return Nothing))
  where
    push_ c x = do
        i <- atomicAddCounter c 1
        return (Just (i, x))

-- | Make a BIO node grouping items into fixed size arrays.
--
newGroupingNode :: Int -> IO (BIO a (A.SmallArray a))
{-# INLINABLE newGroupingNode #-}
newGroupingNode n
    | n < 1 =  newGroupingNode 1
    | otherwise = do
        c <- newCounter 0
        arrRef <- newIORef =<< A.newArr n
        return (BIO (push_ c arrRef) (pull_ c arrRef))
  where
    push_ c arrRef x = do
        i <- readPrimIORef c
        if i == n - 1
        then do
            marr <- readIORef arrRef
            A.writeArr marr i x
            writePrimIORef c 0
            writeIORef arrRef =<< A.newArr n
            return . Just =<< A.unsafeFreezeArr marr
        else do
            marr <- readIORef arrRef
            A.writeArr marr i x
            writePrimIORef c (i+1)
            return Nothing
    pull_ c arrRef = do
        i <- readPrimIORef c
        if i /= 0
        then do
            marr <- readIORef arrRef
#if MIN_VERSION_base(4,14,0)
            A.shrinkMutableArr marr i
            return . Just =<< A.unsafeFreezeArr marr
#else
            marr' <- A.resizeMutableArr marr i
            return . Just =<< A.unsafeFreezeArr marr'
#endif
        else return Nothing

