{-# OPTIONS_GHC -Wno-missing-fields #-}
{-|
Module      : Z.IO.Buffered
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

Some example of such nodes are:

  * Compressor \/ decompressor, e.g. zlib, etc.
  * Ciphers.
  * Codec, e.g. utf8 codec, base64 codec.
  * Packet parsers.

We use @BIO inp out@ type to represent all the objects above, and @BIO Void out@ to represent an 'IO' source,
and @BIO inp ()@ to represent an 'IO' sink, which can all be connected with '>|>' to build a larger 'BIO' node.

-}
module Z.IO.BIO (
  -- * The BIO type
    BIO(..), (>|>), (>+>)
  , concatSource, zipSource, parseSource
  , (>->), concatSink
  , ParseException(..)
  -- * Run BIO chain
  , runSource, runSource_
  , runBlock, runBlock_, unsafeRunBlock
  , runBlocks, runBlocks_, unsafeRunBlocks
  -- * Make new BIO
  , sourceFromList
  , sourceFromBuffer, sourceFromTextBuffer
  , sinkToBuffer
  -- * Input & Output BIO adapters
  , sourceFromInput
  , sourceFromLinedInput
  , sourceFromTextInput
  , sourceFromLinedTextInput
  , sourceFromInputParsed
  , sinkToOutput
  , sinkToOutputAlwaysFlush
  -- * More BIO adapters
  , sourceFromInput'
  , sourceFromLinedInput'
  , sourceFromTextInput'
  , sourceFromLinedTextInput'
  , sourceFromInputParsed'
  , sinkToOutput'
  , sinkToOutputAlwaysFlush'
  ) where

import Control.Monad
import Data.Void
import Data.IORef
import qualified Z.Data.Vector as V
import qualified Z.Data.Text as T
import qualified Z.Data.Parser             as P
import qualified Data.List as List
import Z.IO.Exception
import Z.IO.Buffered
import System.IO.Unsafe     (unsafePerformIO)

-- | A 'BIO'(blocked IO) node.
--
-- A 'BIO' node consist of two functions: 'push' and 'pull'. It can be used to describe different kinds of IO
-- devices:
--
--  * @BIO inp out@ describe an IO state machine(e.g. z_stream in zlib),
--    which takes some input in block, then outputs.
--  * @BIO Void out@ describe an IO source, which never takes input,
--    but gives output until EOF when 'pull'ed.
--  * @BIO inp ()@ describe an IO sink, which takes input and perform some IO effects,
--    such as writing to terminal or files.
--
-- You can connect these 'BIO' nodes with '>|>', which connect left node's output to right node's input, which gives
-- you a new 'BIO' node with left node's input type and right node's output type.
--
-- You can run a 'BIO' node in different ways:
--
--   * 'runSource' will continuously pull value from source, and perform effects along the way.
--   * 'runBlock' will supply a single block of input, and return output if there's any.
--   * 'runBlocks' will supply a list of blocks, and return a list of output blocks.
--
-- Note 'BIO' usually contains some IO states, you can consider it as an opaque 'IORef':
--
--   * You shouldn't reuse a 'BIO' node after connect it to a 'BIO' chain.
--   * You shouldn't use a 'BIO' node across multiple threads.
--
-- Remember 'BIO' is just a convenient way to construct single-thread streaming computation, to use 'BIO'
-- in multiple threads, check "Z.IO.BIO.Concurrent" module.
--
data BIO inp out = BIO
    { push :: HasCallStack => inp -> IO (Maybe out)
      -- ^ push a block of input, perform some effect, and return output,
      -- if input is not enough to produce any output yet, return 'Nothing'.
    , pull :: HasCallStack => IO (Maybe out)
      -- ^ when input reaches EOF, there may a finalize stage to output trailing output blocks.
      -- return 'Nothing' to indicate current node reaches EOF too.
    }

-- | Type alias for 'BIO' node which never takes input.
--
-- 'push' is not available by type system, and 'pull' return 'Nothing' when reaches EOF.
type Source out = BIO Void out

-- | Type alias for 'BIO' node which only takes input and perform effects.
--
-- 'pull' return 'Nothing' when don't want to take more input(usually after flushed).
type Sink inp = BIO inp ()

instance Functor (BIO inp) where
    fmap f BIO{..} = BIO push' pull'
      where
        push' inp = do
            r <- push inp
            return $! fmap f r
        pull' = do
            r <- pull
            return $! fmap f r

-- | Connect two 'BIO' nodes, feed left one's output to right one's input.
(>|>) :: BIO a b -> BIO b c -> BIO a c
{-# INLINE (>|>) #-}
BIO pushA pullA >|> BIO pushB pullB = BIO push' pull'
  where
    push' inp = do
        x <- pushA inp
        case x of Just x' -> pushB x'
                  _       -> return Nothing
    pull' = do
        x <- pullA
        case x of
            Just x' -> do
                y <- pushB x'
                case y of Nothing -> pull'  -- ^ draw input from A until there's an output from B
                          _ -> return y
            _       -> pullB

-- | Connect two 'BIO' source, after first reach EOF, draw element from second.
(>+>) :: Source a -> Source a  -> IO (Source a)
{-# INLINE (>+>) #-}
b1 >+> b2 = concatSource [b1, b2]

-- | Fuse two 'BIO' sink, i.e. everything written to the fused sink will be written to left and right sink.
--
-- Flush result 'BIO' will effectively flush both sink until they all return 'Nothing'.
(>->) :: Sink out -> Sink out -> Sink out
{-# INLINE (>->) #-}
b1 >-> b2 = concatSink [b1, b2]

-- | Fuse a list of 'BIO' sinks, everything written to the fused sink will be written to every sink in the list.
--
-- Flush result 'BIO' will effectively flush every sink until they all return 'Nothing'.
concatSink :: [Sink out] -> Sink out
{-# INLINABLE concatSink #-}
concatSink ss = BIO push' pull'
  where
    push' inp = forM_ ss (\ b -> push b inp) >> return (Just ())
    pull' = loop ss []
    loop (b:bs) acc = do
        r <- pull b
        case r of
            Just _ -> loop bs (b:acc)
            _  -> loop bs acc
    loop [] acc@(_:_) = loop acc []
    loop [] [] = return Nothing

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

-- | Read buffer and parse with 'Parser'.
--
-- This function will continuously draw data from input before parsing finish. Unconsumed
-- bytes will be returned to buffer.
--
-- Return 'Nothing' if reach EOF before parsing, throw 'ParseException' if parsing fail.
parseSource :: HasCallStack => P.Parser a -> Source V.Bytes -> IO (Source a)
{-# INLINABLE parseSource #-}
parseSource p BIO{..} = do
    trailingRef <- newIORef V.empty
    return BIO{ pull = go trailingRef }
  where
    go trailingRef = do
        trailing <- readIORef trailingRef
        if V.null trailing
        then do
            bs <- pull
            case bs of
                Just bs' -> do
                    (rest, r) <- P.parseChunks p s' bs'
                    writeIORef trailingRef rest
                    case r of Right v -> return (Just v)
                              Left e  -> throwIO (ParseException e callStack)
                _    -> return Nothing
        else do
            (rest, r) <- P.parseChunks p s' trailing
            writeIORef trailingRef rest
            case r of Right v -> return (Just v)
                      Left e  -> throwIO (ParseException e callStack)

    s' = pull >>= \ r -> case r of Just r' -> return r'
                                   _       -> return V.empty

-- | Zip two 'BIO' source into one, reach EOF when either one reached EOF.
zipSource :: Source a -> Source b -> Source (a,b)
{-# INLINABLE zipSource #-}
zipSource (BIO _ pullA) (BIO _ pullB) = BIO { pull = do
    mA <- pullA
    mB <- pullB
    return ((,) <$> mA <*> mB)}


-- | Drain a 'BIO' source into a List in memory.
runSource :: Source x -> IO [x]
runSource BIO{..} = loop pull []
  where
    loop f acc = do
        r <- f
        case r of Just r' -> loop f (r':acc)
                  _       -> return (List.reverse acc)

-- | Drain a source without collecting result.
runSource_ :: Source x -> IO ()
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
unsafeRunBlock new inp = unsafePerformIO (new >>= \ bio -> runBlock bio inp)

-- | Supply blocks of input, then run BIO node until EOF.
--
-- Note many 'BIO' node will be closed or not be able to take new input after drained.
runBlocks :: HasCallStack => BIO inp out -> [inp] -> IO [out]
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
unsafeRunBlocks new inps = unsafePerformIO (new >>= \ bio -> runBlocks bio inps)

--------------------------------------------------------------------------------
-- | Turn a 'BufferedOutput' into 'BIO' sink.
--
sinkToBuffer :: BufferedOutput -> Sink V.Bytes
{-# INLINABLE sinkToBuffer #-}
sinkToBuffer bo = BIO push' pull'
  where
    push' inp = writeBuffer bo inp >> pure (Just ())
    pull' = flushBuffer bo >> pure Nothing

-- | Turn an 'Output' into 'BIO' sink.
--
-- 'push' will write input to buffer, and 'pull' will flush buffer.
sinkToOutput :: Output o => o -> IO (Sink V.Bytes)
{-# INLINABLE sinkToOutput #-}
sinkToOutput = sinkToOutput' V.defaultChunkSize

-- | Turn an 'Output' into 'BIO' sink with given buffer size.
--
-- 'push' will write input to buffer, and 'pull' will flush buffer.
sinkToOutput' :: Output o => Int -> o -> IO (Sink V.Bytes)
{-# INLINABLE sinkToOutput' #-}
sinkToOutput' siz o = newBufferedOutput' siz o >>= \ bo -> return (BIO (push' bo) (pull' bo))
  where
    push' bo inp = writeBuffer bo inp >> pure (Just ())
    pull' bo = flushBuffer bo >> pure Nothing

-- | Turn an 'Output' into 'BIO' sink.
--
-- 'push' will write input to buffer then perform flush, tend to degrade performance.
sinkToOutputAlwaysFlush :: Output o => o -> IO (Sink V.Bytes)
{-# INLINABLE sinkToOutputAlwaysFlush #-}
sinkToOutputAlwaysFlush = sinkToOutputAlwaysFlush' V.defaultChunkSize

-- | Turn an 'Output' into 'BIO' sink with given buffer size.
--
-- 'push' will write input to buffer then perform flush, tend to degrade performance
sinkToOutputAlwaysFlush' :: Output o => Int -> o -> IO (Sink V.Bytes)
{-# INLINABLE sinkToOutputAlwaysFlush' #-}
sinkToOutputAlwaysFlush' siz o = newBufferedOutput' siz o >>= \ bo -> return (BIO (push' bo) (pull' bo))
  where
    push' bo inp = do
        writeBuffer bo inp
        flushBuffer bo
        pure (Just ())
    pull' _ = pure Nothing

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

-- | Turn a 'BufferedInput' into 'BIO' source, map EOF to Nothing.
--
sourceFromBuffer :: BufferedInput -> Source V.Bytes
{-# INLINABLE sourceFromBuffer #-}
sourceFromBuffer i = BIO{ pull = do
    readBuffer i >>= \ x -> if V.null x then return Nothing
                                        else return (Just x)}

-- | Turn a UTF8 encoded 'BufferedInput' into 'BIO' source, map EOF to Nothing.
--
sourceFromTextBuffer :: BufferedInput -> Source T.Text
{-# INLINABLE sourceFromTextBuffer #-}
sourceFromTextBuffer i = BIO{ pull = do
    readBufferText i >>= \ x -> if T.null x then return Nothing
                                            else return (Just x)}


-- | Turn input device into a 'V.Bytes' source.
sourceFromInput :: Input i => i -> IO (Source V.Bytes)
sourceFromInput = sourceFromInput' V.defaultChunkSize

-- | Turn input device into a 'V.Bytes' source with given buffer size.
sourceFromInput' :: Input i => Int -> i -> IO (Source V.Bytes)
{-# INLINABLE sourceFromInput' #-}
sourceFromInput' siz i = newBufferedInput' siz i >>= return . sourceFromBuffer

-- | Turn input device into a lined 'V.Bytes' source.
sourceFromLinedInput :: Input i => i -> IO (Source V.Bytes)
sourceFromLinedInput = sourceFromLinedInput' V.defaultChunkSize

-- | Turn input device into a lined 'V.Bytes' source with give buffer size.
sourceFromLinedInput' :: Input i => Int -> i -> IO (Source V.Bytes)
{-# INLINABLE sourceFromLinedInput' #-}
sourceFromLinedInput' siz i = newBufferedInput' siz i >>= \ bi ->
    return BIO{ pull = readLine bi }

-- | Turn input device into a 'T.Text' source.
sourceFromTextInput :: Input i => i -> IO (Source T.Text)
sourceFromTextInput = sourceFromTextInput' V.defaultChunkSize

-- | Turn input device into a 'T.Text' source with give buffer size.
sourceFromTextInput' :: Input i => Int -> i -> IO (Source T.Text)
sourceFromTextInput' siz i = newBufferedInput' (max 4 siz) i >>= return . sourceFromTextBuffer

-- | Turn input device into a lined 'T.Text' source.
sourceFromLinedTextInput :: Input i => i -> IO (Source T.Text)
sourceFromLinedTextInput = sourceFromLinedTextInput' V.defaultChunkSize

-- | Turn input device into a lined 'T.Text' source with give buffer size.
sourceFromLinedTextInput' :: Input i => Int -> i -> IO (Source T.Text)
{-# INLINABLE sourceFromLinedTextInput' #-}
sourceFromLinedTextInput' siz i = newBufferedInput' siz i >>= \ bi ->
    return BIO{ pull = do
        line <- readLine bi
        case line >>= T.validateMaybe of
            Nothing -> throwIO (T.InvalidUTF8Exception callStack)
            r -> return r}

-- | Turn input device into a packet source, see 'sourceInputParsed''.
sourceFromInputParsed :: Input i => i -> P.Parser a -> IO (Source a)
sourceFromInputParsed = sourceFromInputParsed' V.defaultChunkSize

-- | Turn input device into a packet source with given buffer size.
--
-- If you have a device talk in packet format(e.g. continuously send\/receive binary packets),
-- define a packet parser and use this function to get a packet source.
sourceFromInputParsed' :: Input i => Int -> i -> P.Parser a ->  IO (Source a)
{-# INLINABLE sourceFromInputParsed' #-}
sourceFromInputParsed' siz i p = newBufferedInput' siz i >>= return . loopParse
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

-- | Exception when parsing failed in streams.
data ParseException = ParseException P.ParseError CallStack deriving Show
instance Exception ParseException where
    toException = ioExceptionToException
    fromException = ioExceptionFromException
