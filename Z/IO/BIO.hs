{-# OPTIONS_GHC -Wno-missing-fields #-}
{-|
Module      : Z.IO.BIO
Description : Composable IO Loops
Copyright   : (c) Dong Han, 2017-2020
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

    withResource (initSourceFromFile origin) $ \ src ->
        withResource (initSinkToFile target) $ \ sink ->
            runBIO_ $ src >|> base64Enc >|> zlibCompressor >|> sink

> base64AndCompressFile "test" "test.gz"
-- run 'zcat "test.gz" | base64 -d' will give you original file
@

-}
module Z.IO.BIO (
  -- * The BIO type
    BIO, pattern EOF, Source, Sink
  -- ** Basic combinators
  , (>|>), (>~>), (>!>), appendSource, concatSource, concatSource'
  , joinSink, fuseSink
  -- * Run BIO chain
  , discard
  , stepBIO, stepBIO_
  , runBIO, runBIO_
  , runBlock, runBlock_, unsafeRunBlock
  , runBlocks, runBlocks_, unsafeRunBlocks
  -- * Make new BIO
  , pureBIO, ioBIO
  -- ** Source
  , initSourceFromFile
  , initSourceFromFile'
  , sourceFromIO
  , sourceFromList
  , sourceFromBuffered
  , sourceTextFromBuffered
  , sourceJSONFromBuffered
  , sourceParserFromBuffered
  , sourceParseChunksFromBuffered
  -- ** Sink
  , sinkToIO
  , sinkToList
  , initSinkToFile
  , sinkToBuffered
  , sinkBuilderToBuffered
  -- ** Bytes specific
  , newReChunk
  , newUTF8Decoder
  , newParserNode, newMagicSplitter, newLineSplitter
  , newBase64Encoder, newBase64Decoder
  , hexEncoder, newHexDecoder
  -- ** Generic BIO
  , counterNode
  , seqNumNode
  , newGroupingNode
  , ungroupingNode
  , consumedNode
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits              ((.|.))
import           Data.IORef
import qualified Data.List              as List
import           Data.Void
import           Data.Word
import           System.IO.Unsafe       (unsafePerformIO)
import qualified Z.Data.Array           as A
import qualified Z.Data.Builder         as B
import           Z.Data.CBytes          (CBytes)
import qualified Z.Data.JSON            as JSON
import qualified Z.Data.Parser          as P
import           Z.Data.PrimRef
import qualified Z.Data.Text            as T
import qualified Z.Data.Text.UTF8Codec  as T
import qualified Z.Data.Vector          as V
import qualified Z.Data.Vector.Base     as V
import           Z.Data.Vector.Base64
import           Z.Data.Vector.Hex
import           Z.IO.Buffered
import           Z.IO.Exception
import qualified Z.IO.FileSystem.Base   as FS
import           Z.IO.Resource

-- | A 'BIO'(blocked IO) node.
--
-- A 'BIO' node is a push based stream transformer. It can be used to describe different kinds of IO
-- devices:
--
--  * @BIO inp out@ describe an IO state machine(e.g. z_stream in zlib),
--    which takes some input in block, then outputs.
--  * @type Source out = BIO Void out@ described an IO source, which never takes input,
--    but gives output until EOF by looping.
--  * @type Sink inp = BIO inp Void@ described an IO sink, which takes input and perform some IO effects,
--    such as writing to terminal or files.
--
-- You can connect these 'BIO' nodes with '>|>', which connect left node's output to right node's input,
-- and return a new 'BIO' node with left node's input type and right node's output type.
--
-- You can run a 'BIO' node in different ways:
--
--   * 'stepBIO'\/'stepBIO_' to supply a single chunk of input and step the BIO node.
--   * 'runBIO'\/'runBIO_' will supply EOF directly, which will effectively pull all values from source,
--     and push to sink until source reaches EOF.
--   * 'runBlock'\/'runBlock_' will supply a single block of input as whole input and run the BIO node.
--   * 'runBlocks'\/'runBlocks_' will supply a list of blocks as whole input and run the BIO node.
--
-- Note 'BIO' usually contains some IO states, you can consider it as an opaque 'IORef':
--
--   * You shouldn't use a 'BIO' node across multiple 'BIO' chain unless the state can be reset.
--   * You shouldn't use a 'BIO' node across multiple threads unless document states otherwise.
--
-- 'BIO' is simply a convenient way to construct single-thread streaming computation, to use 'BIO'
-- in multiple threads, check "Z.IO.BIO.Concurrent" module.
--
type BIO inp out = Maybe inp                -- ^ 'EOF' indicates upstream reaches EOF
                -> (Maybe out -> IO ())     -- ^ Pass 'EOF' to indicate current node reaches EOF
                -> IO ()

-- | Patterns for more meaningful pattern matching.
pattern EOF :: Maybe a
pattern EOF = Nothing

-- | Connect two 'BIO' nodes, feed left one's output to right one's input.
(>|>) :: BIO a b -> BIO b c -> BIO a c
{-# INLINE (>|>) #-}
f >|> g = \ x k -> f x (\ y -> g y k)

-- | Connect a `BIO` to a pure function: @bio >~> f === bio >|> pureBIO f@
(>~>) :: BIO a b -> (b -> c) -> BIO a c
{-# INLINE (>~>) #-}
f >~> g =  \ x k -> f x (\ y -> k (g <$> y))

-- | Connect BIO to an effectful function: @bio >!> f === bio >|> ioBIO f@
(>!>) :: HasCallStack => BIO a b -> (b -> IO c) -> BIO a c
{-# INLINE (>!>) #-}
f >!> g = \ x k -> f x (\ y -> do
    case y of Just y' -> k . Just =<< g y'
              _ -> k EOF)

-- | Type alias for 'BIO' node which never takes input.
--
-- Note when implement a 'Source', you should assume 'EOF' argument is supplied only once, and you
-- should loop to call downstream continuation with all available chunks, then write a final 'EOF'
-- to indicate EOF.
type Source x = BIO Void x

-- | Type alias for 'BIO' node which only takes input and perform effects.
--
-- Note when implement a 'Sink', you should assume 'EOF' argument is supplied only once(when upstream
-- reaches EOF), you do not need to call downstream continuation before EOF, and
-- do a flush(also write a final 'EOF') when upstream reach EOF.
type Sink x = BIO x Void

-- | Connect two 'BIO' source, after first reach EOF, draw element from second.
appendSource :: HasCallStack => Source a -> Source a -> Source a
{-# INLINE appendSource #-}
b1 `appendSource` b2 = \ _ k -> do
    b1 EOF $ \ y -> do
        case y of Just _ -> k y
                  _      -> b2 EOF k

-- | Fuse two 'BIO' sinks, i.e. everything written to the fused sink will be written to left and right sink.
--
-- Flush result 'BIO' will effectively flush both sink.
joinSink :: HasCallStack => Sink out -> Sink out -> Sink out
{-# INLINE joinSink #-}
b1 `joinSink` b2 = \ mx k ->
    case mx of
        Just _ -> do
            b1 mx discard
            b2 mx discard
        _ -> do
            b1 EOF discard
            b2 EOF discard
            k EOF

-- | Fuse a list of 'BIO' sinks, everything written to the fused sink will be written to every sink in the list.
--
-- Flush result 'BIO' will effectively flush every sink in the list.
fuseSink :: HasCallStack => [Sink out] -> Sink out
{-# INLINABLE fuseSink #-}
fuseSink ss = \ mx k ->
    case mx of
        Just _ -> mapM_ (\ s -> s mx discard) ss
        _ -> do
            mapM_ (\ s -> s mx discard) ss
            k EOF

-- | Connect list of 'BIO' sources, after one reach EOF, draw element from next.
concatSource :: HasCallStack => [Source a] -> Source a
{-# INLINABLE concatSource #-}
concatSource = List.foldl' appendSource emptySource

-- | A 'Source' directly write EOF to downstream.
emptySource :: Source a
{-# INLINABLE emptySource #-}
emptySource = \ _ k -> k EOF

-- | Connect list of 'BIO' sources, after one reach EOF, draw element from next.
concatSource' :: HasCallStack => Source (Source a) -> Source a
{-# INLINABLE concatSource' #-}
concatSource' ssrc = \ _ k -> ssrc EOF $ \ msrc ->
    case msrc of
        Just src -> src EOF $ \ ma ->
            case ma of
                Just _ -> k ma
                _ -> return ()
        _ -> k EOF

-------------------------------------------------------------------------------
-- Run BIO

-- | Discards a value.
discard :: a -> IO ()
{-# INLINABLE discard #-}
discard _ = return ()

-- | Supply a single chunk of input to a 'BIO' and collect result.
stepBIO :: HasCallStack => BIO inp out -> inp -> IO [out]
{-# INLINABLE stepBIO #-}
stepBIO bio inp = do
    accRef <- newIORef []
    bio (Just inp) (mapM_  $ \ x -> modifyIORef' accRef (x:))
    reverse <$> readIORef accRef

-- | Supply a single chunk of input to a 'BIO' without collecting result.
stepBIO_ :: HasCallStack => BIO inp out -> inp -> IO ()
{-# INLINABLE stepBIO_ #-}
stepBIO_ bio inp = bio (Just inp) discard

-- | Run a 'BIO' loop without providing input.
--
-- When used on 'Source', it starts the streaming loop.
-- When used on 'Sink', it performs a flush.
runBIO_ :: HasCallStack => BIO inp out -> IO ()
{-# INLINABLE runBIO_ #-}
runBIO_ bio = bio EOF discard

-- | Run a 'BIO' loop without providing input, and collect result.
--
-- When used on 'Source', it will collect all input chunks.
runBIO :: HasCallStack => BIO inp out -> IO [out]
{-# INLINABLE runBIO #-}
runBIO bio = do
    accRef <- newIORef []
    bio EOF (mapM_  $ \ x -> modifyIORef' accRef (x:))
    reverse <$> readIORef accRef

-- | Run a 'BIO' loop with a single chunk of input and EOF, and collect result.
--
runBlock :: HasCallStack => BIO inp out -> inp -> IO [out]
{-# INLINABLE runBlock #-}
runBlock bio inp = do
    accRef <- newIORef []
    bio (Just inp) (mapM_  $ \ x -> modifyIORef' accRef (x:))
    bio EOF (mapM_  $ \ x -> modifyIORef' accRef (x:))
    reverse <$> readIORef accRef

-- | Run a 'BIO' loop with a single chunk of input and EOF, without collecting result.
--
runBlock_ :: HasCallStack => BIO inp out -> inp -> IO ()
{-# INLINABLE runBlock_ #-}
runBlock_ bio inp = do
    bio (Just inp) discard
    bio EOF discard

-- | Wrap 'runBlock' into a pure interface.
--
-- You can wrap a stateful BIO computation(including the creation of 'BIO' node),
-- when you can guarantee a computation is pure, e.g. compressing, decoding, etc.
unsafeRunBlock :: HasCallStack => IO (BIO inp out) -> inp -> [out]
{-# INLINABLE unsafeRunBlock #-}
unsafeRunBlock new inp = unsafePerformIO (new >>= \ bio -> runBlock bio inp)

-- | Supply blocks of input and EOF to a 'BIO', and collect results.
--
-- Note many 'BIO' node will be closed or not be able to take new input after drained.
runBlocks :: HasCallStack => BIO inp out -> [inp] -> IO [out]
{-# INLINABLE runBlocks #-}
runBlocks bio inps = do
    accRef <- newIORef []
    forM_ inps $ \ inp ->
        bio (Just inp) (mapM_  $ \ x -> modifyIORef' accRef (x:))
    bio EOF (mapM_  $ \ x -> modifyIORef' accRef (x:))
    reverse <$> readIORef accRef

-- | Supply blocks of input and EOF to a 'BIO', without collecting results.
--
-- Note many 'BIO' node will be closed or not be able to take new input after drained.
runBlocks_ :: HasCallStack => BIO inp out -> [inp] -> IO ()
{-# INLINABLE runBlocks_ #-}
runBlocks_ bio inps = do
    forM_ inps $ \ inp ->
        bio (Just inp) discard
    bio EOF discard

-- | Wrap 'runBlocks' into a pure interface.
--
-- Similar to 'unsafeRunBlock', but with a list of input blocks.
unsafeRunBlocks :: HasCallStack => IO (BIO inp out) -> [inp] -> [out]
{-# INLINABLE unsafeRunBlocks #-}
unsafeRunBlocks new inps = unsafePerformIO (new >>= \ bio -> runBlocks bio inps)

-------------------------------------------------------------------------------
-- Source

-- | Source a list(or any 'Foldable') from memory.
--
sourceFromList :: Foldable f => f a -> Source a
sourceFromList xs0 = \ _ k -> do
    mapM_ (k . Just) xs0
    k EOF

-- | Turn a 'BufferedInput' into 'BIO' source, map EOF to EOF.
--
sourceFromBuffered :: HasCallStack => BufferedInput -> Source V.Bytes
{-# INLINABLE sourceFromBuffered #-}
sourceFromBuffered i = \ _ k ->
    let loop = readBuffer i >>= \ x ->
            if V.null x then k EOF else k (Just x) >> loop
    in loop

-- | Turn a `IO` action into 'Source'
sourceFromIO :: HasCallStack => IO (Maybe a) -> Source a
{-# INLINABLE sourceFromIO #-}
sourceFromIO io = \ _ k ->
    let loop = io >>= \ x ->
            case x of
                Just _ -> k x >> loop
                _      -> k EOF
    in loop

-- | Turn a UTF8 encoded 'BufferedInput' into 'BIO' source, map EOF to EOF.
--
sourceTextFromBuffered :: HasCallStack => BufferedInput -> Source T.Text
{-# INLINABLE sourceTextFromBuffered #-}
sourceTextFromBuffered i = \ _ k ->
    let loop = readBufferText i >>= \ x ->
            if T.null x then k EOF else k (Just x) >> loop
    in loop

-- | Turn a 'JSON' encoded 'BufferedInput' into 'BIO' source, ignoring any
-- whitespaces bewteen JSON objects. If EOF reached, then return 'EOF'.
-- Throw 'OtherError' with name "EJSON" if JSON value is not parsed or converted.
--
sourceJSONFromBuffered :: forall a. (JSON.JSON a, HasCallStack) => BufferedInput -> Source a
{-# INLINABLE sourceJSONFromBuffered #-}
sourceJSONFromBuffered = sourceParseChunksFromBuffered JSON.decodeChunks

-- | Turn buffered input device into a packet source, throw 'OtherError' with name @EPARSE@ if parsing fail.
sourceParserFromBuffered :: HasCallStack => P.Parser a -> BufferedInput -> Source a
{-# INLINABLE sourceParserFromBuffered #-}
sourceParserFromBuffered p = sourceParseChunksFromBuffered (P.parseChunks p)

-- | Turn buffered input device into a packet source, throw 'OtherError' with name @EPARSE@ if parsing fail.
sourceParseChunksFromBuffered :: (HasCallStack, T.Print e) => P.ParseChunks IO V.Bytes e a -> BufferedInput -> Source a
{-# INLINABLE sourceParseChunksFromBuffered #-}
sourceParseChunksFromBuffered cp bi = \ _ k ->
    let loopA = do
            bs <- readBuffer bi
            if V.null bs
            then k EOF
            else loopB bs
        loopB bs = do
            (rest, r) <- cp (readBuffer bi) bs
            case r of Right v -> k (Just v)
                      Left e  -> throwOtherError "EPARSE" (T.toText e)
            if V.null rest
            then loopA
            else loopB rest
    in loopA

-- | Turn a file into a 'V.Bytes' source.
initSourceFromFile :: HasCallStack => CBytes -> Resource (Source V.Bytes)
{-# INLINABLE initSourceFromFile #-}
initSourceFromFile p = do
    f <- FS.initFile p FS.O_RDONLY FS.DEFAULT_FILE_MODE
    liftIO (sourceFromBuffered <$> newBufferedInput f)

-- | Turn a file into a 'V.Bytes' source with given chunk size.
initSourceFromFile' :: HasCallStack => CBytes -> Int -> Resource (Source V.Bytes)
{-# INLINABLE initSourceFromFile' #-}
initSourceFromFile' p bufSiz = do
    f <- FS.initFile p FS.O_RDONLY FS.DEFAULT_FILE_MODE
    liftIO (sourceFromBuffered <$> newBufferedInput' bufSiz f)

--------------------------------------------------------------------------------
-- Sink

-- | Turn a 'BufferedOutput' into a 'V.Bytes' sink.
sinkToBuffered :: HasCallStack => BufferedOutput -> Sink V.Bytes
{-# INLINABLE sinkToBuffered #-}
sinkToBuffered bo = \ mbs k ->
    case mbs of
        Just bs -> writeBuffer bo bs
        _       -> flushBuffer bo >> k EOF

-- | Turn a 'BufferedOutput' into a 'B.Builder' sink.
--
sinkBuilderToBuffered :: HasCallStack => BufferedOutput -> Sink (B.Builder a)
{-# INLINABLE sinkBuilderToBuffered #-}
sinkBuilderToBuffered bo = \ mbs k ->
    case mbs of
        Just bs -> writeBuilder bo bs
        _       -> flushBuffer bo >> k EOF

-- | Turn a file into a 'V.Bytes' sink.
--
-- Note the file will be opened in @'FS.O_APPEND' .|. 'FS.O_CREAT' .|. 'FS.O_WRONLY'@ mode,
-- bytes will be written after the end of the original file if there'are old bytes.
initSinkToFile :: HasCallStack => CBytes -> Resource (Sink V.Bytes)
{-# INLINABLE initSinkToFile #-}
initSinkToFile p = do
    f <- FS.initFile p (FS.O_APPEND .|. FS.O_CREAT .|. FS.O_WRONLY) FS.DEFAULT_FILE_MODE
    liftIO (sinkToBuffered <$> newBufferedOutput f)

-- | Turn an `IO` action into 'BIO' sink.
--
sinkToIO :: HasCallStack => (a -> IO ()) -> Sink a
{-# INLINABLE sinkToIO #-}
sinkToIO f = \ ma k ->
    case ma of
        Just a -> f a
        _ -> k EOF

-- | Turn an `IO` action(and a flush action), into 'BIO' sink.
--
sinkToIO' :: HasCallStack => (a -> IO ()) -> IO () -> Sink a
{-# INLINABLE sinkToIO' #-}
sinkToIO' f flush = \ ma k ->
    case ma of
        Just a -> f a
        _ -> flush >> k EOF

-- | Sink to a list in memory.
--
-- The 'MVar' will be empty during sinking, and will be filled after sink receives an EOF.
sinkToList :: IO (MVar [a], Sink a)
sinkToList = do
    xsRef <- newIORef []
    rRef <- newEmptyMVar
    return (rRef, sinkToIO' (\ x -> modifyIORef xsRef (x:))
                            (do modifyIORef xsRef reverse
                                xs <- readIORef xsRef
                                putMVar rRef xs))

--------------------------------------------------------------------------------
-- Nodes

-- | BIO node from a pure function.
--
-- BIO node made with this funtion are stateless, thus can be reused across chains.
pureBIO :: (a -> b) -> BIO a b
{-# INLINE pureBIO #-}
pureBIO f = \ x k -> k (f <$> x)

-- | BIO node from an IO function.
--
-- BIO node made with this funtion may not be stateless, it depends on if the IO function use
-- IO state.
ioBIO :: HasCallStack => (a -> IO b) -> BIO a b
{-# INLINE ioBIO #-}
ioBIO f = \ x k ->
    case x of Just x' -> f x' >>= k . Just
              _ -> k EOF

-- | Make a chunk size divider.
--
-- A divider size divide each chunk's size to the nearest multiplier to granularity,
-- last trailing chunk is directly returned.
newReChunk :: Int                -- ^ chunk granularity
           -> IO (BIO V.Bytes V.Bytes)
{-# INLINABLE newReChunk #-}
newReChunk n = do
    trailingRef <- newIORef V.empty
    return $ \ mbs k ->
        case mbs of
            Just bs -> do
                trailing <- readIORef trailingRef
                let chunk =  trailing `V.append` bs
                    l = V.length chunk
                if l >= n
                then do
                    let l' = l - (l `rem` n)
                        (chunk', rest) = V.splitAt l' chunk
                    writeIORef trailingRef rest
                    k (Just chunk')
                else writeIORef trailingRef chunk
            _ -> do
                trailing <- readIORef trailingRef
                unless (V.null trailing) $ do
                    writeIORef trailingRef V.empty
                    k (Just trailing)
                k EOF

-- | Read buffer and parse with 'Parser'.
--
-- This function will turn a 'Parser' into a 'BIO', throw 'OtherError' with name @EPARSE@ if parsing fail.
--
newParserNode :: HasCallStack => P.Parser a -> IO (BIO V.Bytes a)
{-# INLINABLE newParserNode #-}
newParserNode p = do
    -- type LastParseState = Maybe (V.Bytes -> P.Result)
    resultRef <- newIORef EOF
    return $ \ minp k -> do
        let loop f chunk = case f chunk of
                P.Success a trailing -> do
                    k (Just a)
                    unless (V.null trailing) (loop f trailing)
                P.Partial f' ->
                    writeIORef resultRef (Just f')
                P.Failure e _ ->
                    throwOtherError "EPARSE" (T.toText e)
        lastResult <- readIORef resultRef
        case minp of
            Just bs -> do
                let f = case lastResult of
                        Just x    -> x
                        _         -> P.parseChunk p
                loop f bs
            _ ->
                case lastResult of
                    Just f -> loop f V.empty
                    _ -> k EOF

-- | Make a new UTF8 decoder, which decode bytes streams into text streams.
--
-- If there're invalid UTF8 bytes, an 'OtherError' with name 'EINVALIDUTF8' will be thrown.`
--
-- Note this node is supposed to be used with preprocess node such as decompressor, parser, etc.
-- where bytes boundary cannot be controlled, UTF8 decoder will concat trailing bytes from last block to next one.
-- Use this node directly with 'sourceFromBuffered' will not be as efficient as directly use
-- 'sourceTextFromBuffered', because 'BufferedInput' provides push back capability,
-- trailing bytes can be pushed back to reading buffer then returned with next block input together.
--
newUTF8Decoder :: HasCallStack => IO (BIO V.Bytes T.Text)
{-# INLINABLE newUTF8Decoder #-}
newUTF8Decoder = do
    trailingRef <- newIORef V.empty
    return $ \ mbs k -> do
        case mbs of
            Just bs -> do
                trailing <- readIORef trailingRef
                let chunk =  trailing `V.append` bs
                    (V.PrimVector arr s l) = chunk
                if l > 0 && T.decodeCharLen arr s <= l
                then do
                    let (i, _) = V.findR (\ w -> w >= 0b11000000 || w <= 0b01111111) chunk
                    if (i == -1)
                    then throwOtherError "EINVALIDUTF8" "invalid UTF8 bytes"
                    else do
                        if T.decodeCharLen arr (s + i) > l - i
                        then do
                            writeIORef trailingRef (V.fromArr arr (s+i) (l-i))
                            k (Just (T.validate (V.fromArr arr s i)))
                        else do
                            writeIORef trailingRef V.empty
                            k (Just (T.validate chunk))
                else writeIORef trailingRef chunk

            _ -> do
                trailing <- readIORef trailingRef
                if V.null trailing
                then k EOF
                else throwOtherError "EINVALIDUTF8" "invalid UTF8 bytes"

-- | Make a new stream splitter based on magic byte.
--
newMagicSplitter :: Word8 -> IO (BIO V.Bytes V.Bytes)
{-# INLINABLE newMagicSplitter #-}
newMagicSplitter magic = do
    trailingRef <- newIORef V.empty
    return $ \ mx k ->
        case mx of
            Just bs -> do
                trailing <- readIORef trailingRef
                let loop chunk = case V.elemIndex magic chunk of
                        Just i -> do
                            -- TODO: looping
                            let (line, rest) = V.splitAt (i+1) chunk
                            k (Just line)
                            loop rest
                        _ -> writeIORef trailingRef chunk
                loop (trailing `V.append` bs)
            _ -> do
                chunk <- readIORef trailingRef
                unless (V.null chunk) $ do
                    writeIORef trailingRef V.empty
                    k (Just chunk)
                k EOF

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
            Just r | r == 13   -> V.PrimVector arr s (l-2)
                   | otherwise -> V.PrimVector arr s (l-1)
            _ | V.head bs == 10 -> V.PrimVector arr s (l-1)
              | otherwise -> V.PrimVector arr s l

-- | Make a new base64 encoder node.
newBase64Encoder :: IO (BIO V.Bytes V.Bytes)
{-# INLINABLE newBase64Encoder #-}
newBase64Encoder = do
    re <- newReChunk 3
    return (re >~> base64Encode)

-- | Make a new base64 decoder node.
newBase64Decoder :: HasCallStack => IO (BIO V.Bytes V.Bytes)
{-# INLINABLE newBase64Decoder #-}
newBase64Decoder = do
    re <- newReChunk 4
    return (re >~> base64Decode')

-- | Make a hex encoder node.
--
-- Hex encoder is stateless, it can be reused across chains.
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
-- 'Counter' is increased atomically, it's safe to read \/ reset the counter from other threads.
counterNode :: Counter -> BIO a a
{-# INLINABLE counterNode #-}
counterNode c = ioBIO inc
  where
    inc x = do
        atomicAddCounter_ c 1
        return x

-- | Make a new BIO node which counts items, and label item with a sequence number.
--
-- 'Counter' is increased atomically, it's safe to read \/ reset the counter from other threads.
seqNumNode :: Counter -> BIO a (Int, a)
{-# INLINABLE seqNumNode #-}
seqNumNode c = ioBIO inc
  where
    inc x = do
        i <- atomicAddCounter c 1
        return (i, x)

-- | Make a BIO node grouping items into fixed size arrays.
--
-- Trailing items are directly returned.
newGroupingNode :: Int -> IO (BIO a (V.Vector a))
{-# INLINABLE newGroupingNode #-}
newGroupingNode n
    | n < 1 =  newGroupingNode 1
    | otherwise = do
        c <- newCounter 0
        arrRef <- newIORef =<< A.newArr n
        return $ \ mx k ->
            case mx of
                Just x -> do
                    i <- readPrimIORef c
                    if i == n - 1
                    then do
                        marr <- readIORef arrRef
                        A.writeArr marr i x
                        writePrimIORef c 0
                        writeIORef arrRef =<< A.newArr n
                        arr <- A.unsafeFreezeArr marr
                        k . Just $! V.fromArr arr 0 n
                    else do
                        marr <- readIORef arrRef
                        A.writeArr marr i x
                        writePrimIORef c (i+1)
                _ -> do
                    i <- readPrimIORef c
                    if i /= 0
                    then do
                        writePrimIORef c 0
                        marr <- readIORef arrRef
                        A.shrinkMutableArr marr i
                        arr <- A.unsafeFreezeArr marr
                        k . Just $! V.fromArr arr 0 i
                    else k EOF

-- | A BIO node flatten items.
--
ungroupingNode :: BIO (V.Vector a) a
{-# INLINABLE ungroupingNode #-}
ungroupingNode = \ mx k ->
    case mx of
        Just x -> V.traverseVec_ (k . Just) x
        _      -> k EOF

-- | A BIO node which write 'True' to 'IORef' when 'EOF' is reached.
consumedNode :: IORef Bool -> BIO a a
{-# INLINABLE consumedNode #-}
consumedNode ref = \ mx k -> case mx of
    Just _ -> k mx
    _ -> do writeIORef ref True
            k EOF


