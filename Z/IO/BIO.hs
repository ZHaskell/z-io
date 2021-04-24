{-# OPTIONS_GHC -Wno-missing-fields #-}
{-|
Module      : Z.IO.BIO
Description : Buffered IO interface
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
            runBIO $ src >|> base64Enc >|> zlibCompressor >|> sink

> base64AndCompressFile "test" "test.gz"
-- run 'zcat "test.gz" | base64 -d' will give you original file
@

-}
module Z.IO.BIO (
  -- * The BIO type
    BIO(..), Source, pattern Source, Sink
  , Step(..), returnStep, stepIO, foldStep, foldStepIO
  -- ** Basic combinators
  , (>|>), (>~>), (>!>), appendSource
  , concatSource, zipSource
  , joinSink, fuseSink
  -- * Run BIO chain
  , runBIO
  , runSource, runSource_
  , runBlock, runBlock_, unsafeRunBlock
  , runBlocks, runBlocks_, unsafeRunBlocks
  -- * Make new BIO
  , pureBIO, ioBIO
  -- ** Source
  , sourceFromIO
  , sourceToIO
  , sourceFromList
  , initSourceFromFile
  , initSourceFromFile'
  , initTextSourceFromFile
  , sourceFromBuffered
  , sourceTextFromBuffered
  , sourceJSONFromBuffered
  , sourceParserFromBuffered
  , sourceParseChunkFromBuffered
  -- ** Sink
  , sinkToIO
  , sinkToList
  , initSinkToFile
  , sinkToBuffered
  , sinkBuilderToBuffered
  , sinkToBuffered'
  , sinkBuilderToBuffered'
  -- ** Bytes specific
  , newParseChunkNode, newReChunk, newUTF8Decoder, newMagicSplitter, newLineSplitter
  , newBase64Encoder, newBase64Decoder
  , hexEncoder, newHexDecoder
  -- ** Generic BIO
  , newCounterNode
  , newSeqNumNode
  , newGroupingNode
  , ungroupingNode
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
import qualified Z.Data.Vector.Extra    as V
import           Z.Data.Vector.Base64
import           Z.Data.Vector.Hex
import           Z.IO.Buffered
import           Z.IO.Exception
import qualified Z.IO.FileSystem.Base   as FS
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
--   * You shouldn't use a 'BIO' node across multiple 'BIO' chain unless the state can be reset.
--   * You shouldn't use a 'BIO' node across multiple threads unless document states otherwise.
--
-- 'BIO' is simply a convenient way to construct single-thread streaming computation, to use 'BIO'
-- in multiple threads, check "Z.IO.BIO.Concurrent" module.
--
-- When implement a BIO node, you should consume as much input as possible in 'push', and offload
-- the input state to 'Step' until you can't produce any output.
--
data BIO inp out = BIO
    { push :: inp -> IO (Step out)
      -- ^ Push a block of input, perform some effect, and return output,
      -- if input is not enough to produce any output yet, return 'Stop'.
    , pull :: IO (Step out)
      -- ^ When input reaches EOF, there may be a finalize stage to output
      -- trailing output blocks. return 'Stop' to indicate current node
      -- reaches EOF too.
    }

-- | Result of running a single BIO step.
--
-- 'Stop' has different meaning in 'push' and 'pull':
--
--   * In 'push' a 'Stop' means current input chunk is not able to produce any output chunk.
--   * In 'pull' a 'Stop' means the current BIO node reaches its EOF.
--
data Step a = Step !a (IO (Step a)) | Stop

instance Show a => Show (Step a) where
    show (Step x _) = "Step " ++ show x ++ " _"
    show _ = "Stop"

instance Functor Step where
    {-# INLINE fmap #-}
    fmap f (Step x g) = Step (f x) (return . fmap f =<< g)
    fmap _ Stop = Stop

-- | Apply an `IO` function in each 'Step'.
stepIO :: (a -> IO b) -> Step a -> IO (Step b)
{-# INLINE stepIO #-}
stepIO f (Step x g) = do
    y <- f x
    return (Step y (stepIO f =<< g))
stepIO _ _ = return Stop

-- | Use an `IO` function to fold each 'Step'.
foldStep :: (b -> a -> b) -> b -> Step a -> IO b
{-# INLINE foldStep #-}
foldStep f acc0 s = go acc0 s
  where
    go !acc (Step x g) = go (f acc x) =<< g
    go !acc _ = return acc

-- | Use an `IO` function to fold each 'Step'.
foldStepIO :: (b -> a -> IO b) -> b -> Step a -> IO b
{-# INLINE foldStepIO #-}
foldStepIO f acc0 s = go acc0 s
  where
    go !acc (Step x g) = do
        acc' <- f acc x
        go acc' =<< g
    go !acc _ = return acc

-- | Materialize 'Step' s into memory.
runStep :: Step a -> IO [a]
{-# INLINE runStep #-}
runStep s = List.reverse <$> foldStep (\ acc x -> x:acc) [] s

-- | Run 'Step' s without collecting results.
runStep_ :: Step a -> IO ()
{-# INLINE runStep_ #-}
runStep_ = loop
  where
    loop (Step _ g) = loop =<< g
    loop _          = return ()

-- | Return a single result in one step.
returnStep :: a -> IO (Step a)
{-# INLINE returnStep #-}
returnStep x = return (Step x (return Stop))

-- | Type alias for 'BIO' node which never takes input.
--
-- 'push' is not available by type system, and 'pull' return 'Stop' when reaches EOF.
type Source out = BIO Void out

pattern Source :: IO (Step a) -> Source a
pattern Source x = BIO{ pull = x}
{-# COMPLETE Source #-}

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
(>|>) :: HasCallStack => BIO a b -> BIO b c -> BIO a c
{-# INLINE (>|>) #-}
BIO pushA pullA >|> BIO pushB pullB =
    BIO (loopA <=< pushA) (loopA =<< pullA)
  where
    loopA x = case x of
        Step x' f -> loopB f =<< pushB x'
        _         -> pullB
    loopB f y = case y of
        Step y' g -> return (Step y' (loopB f =<< g))
        _ -> loopA =<< f

-- | Flipped 'fmap' for easier chaining.
(>~>) :: BIO a b -> (b -> c) -> BIO a c
{-# INLINE (>~>) #-}
(>~>) = flip fmap

-- | Connect BIO to an effectful function.
(>!>) :: HasCallStack => BIO a b -> (b -> IO c) -> BIO a c
{-# INLINE (>!>) #-}
(>!>) BIO{..} f = BIO (push >=> stepIO f) (pull >>= stepIO f)

-- | Connect two 'BIO' source, after first reach EOF, draw element from second.
appendSource :: HasCallStack => Source a -> Source a  -> Source a
{-# INLINE appendSource #-}
b1 `appendSource` b2 = concatSource [b1, b2]

-- | Fuse two 'BIO' sinks, i.e. everything written to the fused sink will be written to left and right sink.
--
-- Flush result 'BIO' will effectively flush both sink.
joinSink :: HasCallStack => Sink out -> Sink out -> Sink out
{-# INLINE joinSink #-}
b1 `joinSink` b2 = fuseSink [b1, b2]

-- | Fuse a list of 'BIO' sinks, everything written to the fused sink will be written to every sink in the list.
--
-- Flush result 'BIO' will effectively flush every sink in the list.
fuseSink :: HasCallStack => [Sink out] -> Sink out
{-# INLINABLE fuseSink #-}
fuseSink ss = BIO push_ pull_
  where
    push_ inp = forM_ ss (\ b -> push b inp) >> return Stop
    pull_ = mapM_ pull ss >> return Stop

-- | Connect list of 'BIO' sources, after one reach EOF, draw element from next.
concatSource :: HasCallStack => [Source a] -> Source a
{-# INLINABLE concatSource #-}
concatSource ss0 = Source (loopA ss0)
  where
    loopA [] = return Stop
    loopA (s:ss) = loopB ss =<< pull s

    loopB ss x = case x of
        Step x' f -> return (Step x' (loopB ss =<< f))
        _         -> loopA ss

-- | Zip two 'BIO' source into one, reach EOF when either one reached EOF.
zipSource :: HasCallStack => Source a -> Source b -> Source (a,b)
{-# INLINABLE zipSource #-}
zipSource (Source f) (Source g) = Source (do x <- f; y <- g; loop x y)
  where
    loop Stop _ = return Stop
    loop _ Stop = return Stop
    loop (Step a f') (Step b g') =
        return (Step (a, b) (do x <- f'; y <- g'; loop x y))

-------------------------------------------------------------------------------
-- Run BIO

-- | Run a 'BIO' loop (source >|> ... >|> sink).
runBIO :: HasCallStack => BIO Void Void -> IO ()
{-# INLINABLE runBIO #-}
runBIO BIO{..} = void pull

-- | Drain a 'BIO' source into a List in memory.
runSource :: HasCallStack => Source x -> IO [x]
{-# INLINABLE runSource #-}
runSource BIO{..} = runStep =<< pull

-- | Drain a source without collecting result.
runSource_ :: HasCallStack => Source x -> IO ()
{-# INLINABLE runSource_ #-}
runSource_ BIO{..} = runStep_ =<< pull

-- | Supply a single block of input, then run BIO node until EOF.
--
-- Note many 'BIO' node will be closed or not be able to take new input after drained.
runBlock :: HasCallStack => BIO inp out -> inp -> IO [out]
{-# INLINABLE runBlock #-}
runBlock BIO{..} inp = do
    acc' <- foldStep (\ acc x -> x:acc) [] =<< push inp
    acc''<- foldStep (\ acc x -> x:acc) acc' =<< pull
    return (List.reverse acc'')

-- | Supply a single block of input, then run BIO node until EOF with collecting result.
--
-- Note many 'BIO' node will be closed or not be able to take new input after drained.
runBlock_ :: HasCallStack => BIO inp out -> inp -> IO ()
{-# INLINABLE runBlock_ #-}
runBlock_ BIO{..} inp = do
    runStep_ =<< push inp
    runStep_ =<< pull

-- | Wrap 'runBlock' into a pure interface.
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
runBlocks BIO{..} = loopA []
  where
    loopA acc (inp:inps) = do
        acc' <- (loopB acc) =<< push inp
        loopA acc' inps

    loopA acc _ = do
        acc' <- (loopB acc) =<< pull
        return (List.reverse acc')

    loopB = foldStep (\ acc x -> x:acc)

-- | Supply blocks of input, then run BIO node until EOF with collecting result.
--
-- Note many 'BIO' node will be closed or not be able to take new input after drained.
runBlocks_ :: HasCallStack => BIO inp out -> [inp] -> IO ()
{-# INLINABLE runBlocks_ #-}
runBlocks_ BIO{..} = loop
  where
    loop (inp:inps) = do
        runStep_ =<< push inp
        loop inps
    loop _ = runStep_ =<< pull

-- | Wrap 'runBlocks' into a pure interface.
--
-- Similar to 'unsafeRunBlock', but with a list of input blocks.
unsafeRunBlocks :: HasCallStack => IO (BIO inp out) -> [inp] -> [out]
{-# INLINABLE unsafeRunBlocks #-}
unsafeRunBlocks new inps = unsafePerformIO (new >>= \ bio -> runBlocks bio inps)

-------------------------------------------------------------------------------
-- Source

-- | Source a list from memory.
--
-- Note this is a stateless source, which can be used in multiple BIO chain to source
-- the list again.
sourceFromList :: [a] -> Source a
sourceFromList = Source . loop
  where
    loop (x:xs') = return (Step x (loop xs'))
    loop _       = return Stop

-- | Turn a 'BufferedInput' into 'BIO' source.
--
sourceFromBuffered :: HasCallStack => BufferedInput -> Source V.Bytes
{-# INLINABLE sourceFromBuffered #-}
sourceFromBuffered i = Source (readBuffer i >>= loop)
  where
    loop x = if V.null x then return Stop
                         else return (Step x (readBuffer i >>= loop))

-- | Turn a `IO` action into 'Source'
sourceFromIO :: HasCallStack => IO (Maybe a) -> Source a
{-# INLINABLE sourceFromIO #-}
sourceFromIO io = Source (io >>= loop)
  where
    loop (Just x) = return (Step x (loop =<< io))
    loop (_     ) = return Stop

-- | Turn a `Source` into an `IO` action.
sourceToIO :: Source a -> IO (IO (Maybe a))
{-# INLINABLE sourceToIO #-}
sourceToIO (Source src) = do
    fRef <- newIORef src
    return $ do
        f <- readIORef fRef
        r <- f
        case r of
            Step x f' -> do
                writeIORef fRef f'
                return (Just x)
            _ -> return Nothing

-- | Turn a UTF8 encoded 'BufferedInput' into 'BIO' source.
--
sourceTextFromBuffered :: HasCallStack => BufferedInput -> Source T.Text
{-# INLINABLE sourceTextFromBuffered #-}
sourceTextFromBuffered i = Source (readBufferText i >>= loop)
  where
    loop x = if T.null x then return Stop
                         else return (Step x (readBufferText i >>= loop))

-- | Turn a 'JSON' encoded 'BufferedInput' into 'BIO' source, ignoring any
-- whitespaces bewteen JSON objects.
--
-- Throw 'OtherError' with name "EJSON" if JSON value is not parsed or converted.
sourceJSONFromBuffered :: forall a. (JSON.JSON a, HasCallStack)
                       => BufferedInput -> Source a
{-# INLINABLE sourceJSONFromBuffered #-}
sourceJSONFromBuffered = sourceParseChunkFromBuffered JSON.decodeChunk

-- | Turn buffered input device into a packet source, throw 'OtherError' with name @EPARSE@ if parsing fail.
sourceParserFromBuffered :: HasCallStack => P.Parser a -> BufferedInput -> Source a
{-# INLINABLE sourceParserFromBuffered #-}
sourceParserFromBuffered p = sourceParseChunkFromBuffered (P.parseChunk p)

-- | Turn buffered input device into a packet source, throw 'OtherError' with name @EPARSE@ if parsing fail.
sourceParseChunkFromBuffered :: (HasCallStack, T.Print e)
                             => (V.Bytes -> P.Result e a)
                             -> BufferedInput
                             -> Source a
{-# INLINABLE sourceParseChunkFromBuffered #-}
sourceParseChunkFromBuffered pc bi = Source (loop =<< readBuffer bi)
  where
    loop bs =
        if V.null bs
        then return Stop
        else do
           (rest, r) <- P.parseChunks pc (readBuffer bi) bs
           unReadBuffer rest bi
           case r of Right v -> return (Step v (loop =<< readBuffer bi))
                     Left e  -> throwOtherError "EPARSE" (T.toText e)

-- | Turn a file into a 'V.Bytes' source.
initSourceFromFile :: HasCallStack => CBytes -> Resource (Source V.Bytes)
{-# INLINABLE initSourceFromFile #-}
initSourceFromFile p = do
    f <- FS.initFile p FS.O_RDONLY FS.DEFAULT_FILE_MODE
    liftIO (sourceFromBuffered <$> newBufferedInput f)

-- | Turn a file into a 'V.Bytes' source.
initSourceFromFile' :: HasCallStack => Int -> CBytes -> Resource (Source V.Bytes)
{-# INLINABLE initSourceFromFile' #-}
initSourceFromFile' bufSiz p = do
    f <- FS.initFile p FS.O_RDONLY FS.DEFAULT_FILE_MODE
    liftIO (sourceFromBuffered <$> newBufferedInput' bufSiz f)

-- | Turn a file into a 'T.Text' source.
initTextSourceFromFile :: HasCallStack => CBytes -> Resource (Source T.Text)
{-# INLINABLE initTextSourceFromFile #-}
initTextSourceFromFile p = do
    f <- FS.initFile p FS.O_RDONLY FS.DEFAULT_FILE_MODE
    liftIO (sourceTextFromBuffered <$> newBufferedInput f)

--------------------------------------------------------------------------------
-- Sink

-- | Turn a 'BufferedOutput' into a 'V.Bytes' sink, flush after EOF reached.
sinkToBuffered :: HasCallStack => BufferedOutput -> Sink V.Bytes
{-# INLINABLE sinkToBuffered #-}
sinkToBuffered bo = BIO push_ pull_
  where
    push_ inp = writeBuffer bo inp >> pure Stop
    pull_ = flushBuffer bo >> pure Stop

-- | Turn a 'BufferedOutput' into a 'V.Bytes' sink, flush after each block get pushed.
sinkToBuffered' :: HasCallStack => BufferedOutput -> Sink V.Bytes
{-# INLINABLE sinkToBuffered' #-}
sinkToBuffered' bo = BIO push_ pull_
  where
    push_ inp = writeBuffer bo inp >> flushBuffer bo >> pure Stop
    pull_ = pure Stop

-- | Turn a 'BufferedOutput' into a 'B.Builder' sink, flush after EOF reached.
--
sinkBuilderToBuffered :: HasCallStack => BufferedOutput -> Sink (B.Builder a)
{-# INLINABLE sinkBuilderToBuffered #-}
sinkBuilderToBuffered bo = BIO push_ pull_
  where
    push_ inp = writeBuilder bo inp >> pure Stop
    pull_ = flushBuffer bo >> pure Stop

-- | Turn a 'BufferedOutput' into a 'B.Builder' sink, flush after each builder get pushed.
--
sinkBuilderToBuffered' :: HasCallStack => BufferedOutput -> Sink (B.Builder a)
{-# INLINABLE sinkBuilderToBuffered' #-}
sinkBuilderToBuffered' bo = BIO push_ pull_
  where
    push_ inp = writeBuilder bo inp >> flushBuffer bo >> pure Stop
    pull_ = pure Stop

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
-- 'push' will call `IO` action with input chunk, `pull` has no effect.
sinkToIO :: HasCallStack => (a -> IO ()) -> Sink a
{-# INLINABLE sinkToIO #-}
sinkToIO f = BIO push_ pull_
  where
    push_ x = f x >> pure Stop
    pull_ = pure Stop

-- | Sink to a list in memory.
--
sinkToList :: IO (MVar [a], Sink a)
sinkToList = do
    xsRef <- newIORef []
    rRef <- newEmptyMVar
    return (rRef, BIO (\ x -> modifyIORef xsRef (x:) >> return Stop)
                       (do xs <- readIORef xsRef
                           putMVar rRef (reverse xs)
                           return Stop))

--------------------------------------------------------------------------------
-- Nodes

-- | BIO node from a pure function.
--
-- BIO node made with this funtion are stateless, thus can be reused across chains.
pureBIO :: (a -> b) -> BIO a b
pureBIO f = BIO push_ pull_
  where
    push_ x = returnStep (f x)
    pull_ = return Stop

-- | BIO node from an IO function.
--
-- BIO node made with this funtion may not be stateless, it depends on if the IO function use
-- IO state.
ioBIO :: (HasCallStack => a -> IO b) -> BIO a b
ioBIO f = BIO push_ pull_
  where
    push_ x = returnStep =<< f x
    pull_   = return Stop

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
    push_ trailingRef = \ bs -> do
        trailing <- readIORef trailingRef
        let chunk =  trailing `V.append` bs
            l = V.length chunk
        if l >= n
        then do
            let l' = l - (l `rem` n)
                (chunk', rest) = V.splitAt l' chunk
            writeIORef trailingRef rest
            returnStep chunk'
        else do
            writeIORef trailingRef chunk
            return Stop

    pull_ trailingRef = do
        trailing <- readIORef trailingRef
        if V.null trailing
        then return Stop
        else do
            writeIORef trailingRef V.empty
            returnStep trailing

-- | A node that transform 'V.Bytes' into data using 'P.parseChunk' style functions.
--
newParseChunkNode :: (HasCallStack, T.Print e) => (V.Bytes -> P.Result e a) -> IO (BIO V.Bytes a)
{-# INLINABLE newParseChunkNode #-}
newParseChunkNode pc = do
    -- type LastParseState = Either V.Bytes (V.Bytes -> P.Result)
    fRef <- newIORef Nothing
    return (BIO (push_ fRef) (pull_ fRef))
  where
    push_ fRef chunk = do
        mf <- readIORef fRef
        loop fRef (maybe pc id mf) chunk

    loop fRef f chunk = do
        case f chunk of
            P.Success a chunk' ->
                return (Step a (loop fRef f chunk'))
            P.Failure e _ ->
                throwOtherError "EPARSE" (T.toText e)
            P.Partial f' -> do
                writeIORef fRef (Just f')
                return Stop

    pull_ fRef = do
        f <- readIORef fRef
        case f of
            Just f' -> case f' V.empty of
                P.Success a _ ->
                    returnStep a
                P.Failure e _ ->
                    throwOtherError "EPARSE" (T.toText e)
                P.Partial _ ->
                    throwOtherError "EPARSE" "last chunk partial parse"
            _ -> return Stop

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
            then throwOtherError "EINVALIDUTF8" "invalid UTF8 bytes"
            else do
                if T.decodeCharLen arr (s + i) > l - i
                then do
                    writeIORef trailingRef (V.fromArr arr (s+i) (l-i))
                    returnStep (T.validate (V.fromArr arr s i))
                else do
                    writeIORef trailingRef V.empty
                    returnStep (T.validate chunk)
        else do
            writeIORef trailingRef chunk
            return Stop

    pull_ trailingRef = do
        trailing <- readIORef trailingRef
        if V.null trailing
        then return Stop
        else throwOtherError "EINVALIDUTF8" "invalid UTF8 bytes"

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
        loop trailingRef (trailing `V.append` bs)

    loop trailingRef chunk =
        case V.elemIndex magic chunk of
            Just i -> do
                let (line, rest) = V.splitAt (i+1) chunk
                return (Step line (loop trailingRef rest))
            _ -> do
                writeIORef trailingRef chunk
                return Stop

    pull_ trailingRef = do
        chunk <- readIORef trailingRef
        if V.null chunk
        then return Stop
        else do
            writeIORef trailingRef V.empty
            returnStep chunk

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
-- Returned 'Counter' is increased atomically, it's safe to read \/ reset the counter from other threads.
newCounterNode :: IO (Counter, BIO a a)
{-# INLINABLE newCounterNode #-}
newCounterNode = do
    c <- newCounter 0
    return (c, BIO (push_ c) (return Stop))
  where
    push_ c x = do
        atomicAddCounter_ c 1
        returnStep x

-- | Make a new BIO node which counts items, and label item with a sequence number.
--
-- Returned 'Counter' is increased atomically, it's safe to read \/ reset the counter from other threads.
newSeqNumNode :: IO (Counter, BIO a (Int, a))
{-# INLINABLE newSeqNumNode #-}
newSeqNumNode = do
    c <- newCounter 0
    return (c, BIO (push_ c) (return Stop))
  where
    push_ c x = do
        !i <- atomicAddCounter c 1
        returnStep (i, x)

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
            arr <- A.unsafeFreezeArr marr
            returnStep (V.fromArr arr 0 n)
        else do
            marr <- readIORef arrRef
            A.writeArr marr i x
            writePrimIORef c (i+1)
            return Stop
    pull_ c arrRef = do
        i <- readPrimIORef c
        if i /= 0
        then do
            writePrimIORef c 0
            marr <- readIORef arrRef
            A.shrinkMutableArr marr i
            arr <- A.unsafeFreezeArr marr
            returnStep (V.fromArr arr 0 i)
        else return Stop

-- | Make a BIO node flatten items.
--
ungroupingNode :: BIO (V.Vector a) a
{-# INLINABLE ungroupingNode #-}
ungroupingNode = BIO push_ pull_
  where
    push_ v = do
        if V.null v
        then return Stop
        else return (Step (V.unsafeHead v) (push_ (V.unsafeTail v)))

    pull_ = return Stop
