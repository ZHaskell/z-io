{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Z.IO.Buffered
Description : Buffered IO interface
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide buffered IO interface.

-}

module Z.IO.Buffered
  ( -- * Input & Output device
    Input(..), Output(..)
    -- * Buffered Input
  , BufferedInput
  , newBufferedInput
  , readBuffer
  , unReadBuffer
  , readParser
  , readExactly,  readExactly'
  , readToMagic, readToMagic'
  , readLine, readLine'
  , readAll, readAll'
    -- * Buffered Output
  , BufferedOutput
  , newBufferedOutput
  , writeBuffer
  , writeBuilder
  , flushBuffer
    -- * Stream utilities
  , Source, Sink
  , sourceBuffer
  , sinkBuffer
  , sourceFromList
  , (>+>)
  , parseSource
  , collectSource
  , concatSource
  , zipSource
  , (>>>>=)

    -- * Exceptions
  , BufferedException(..)
    -- * common buffer size
  , V.defaultChunkSize
  , V.smallChunkSize
  ) where

import           Control.Monad
import           Control.Monad.Primitive     (ioToPrim, primToIO)
import           Control.Monad.ST
import           Data.IORef
import           Data.Primitive.PrimArray
import           Data.Typeable
import           Data.Word
import           Data.Bits                 (unsafeShiftR)
import           Foreign.Ptr
import           Z.Data.Array
import qualified Z.Data.Builder.Base       as B
import qualified Z.Data.Parser             as P
import qualified Z.Data.Vector             as V
import qualified Z.Data.Vector.Base        as V
import           Z.Data.PrimRef.PrimIORef
import           Z.Foreign
import           Z.IO.Exception

-- | Input device
--
-- 'readInput' should return 0 on EOF.
--
class Input i where
    readInput :: HasCallStack => i -> Ptr Word8 -> Int -> IO Int

-- | Output device
--
-- 'writeOutput' should not return until all data are written (may not
-- necessarily flushed to hardware, that should be done in device specific way).
--
class Output o where
    writeOutput :: HasCallStack => o -> Ptr Word8 -> Int -> IO ()

-- | Input device with buffer, NOT THREAD SAFE!
--
-- * A 'BufferedInput' should not be used in multiple threads, there's no locking mechanism to protect
--   buffering state.
--
-- * A 'Input' device should only be used with a single 'BufferedInput', If multiple 'BufferedInput' s
--   are opened on a same 'Input' device, the behaviour is undefined.
--
data BufferedInput i = BufferedInput
    { bufInput    :: i
    , bufPushBack :: {-# UNPACK #-} !(IORef V.Bytes)
    , inputBuffer :: {-# UNPACK #-} !(IORef (MutablePrimArray RealWorld Word8))
    }

-- | Output device with buffer, NOT THREAD SAFE!
--
-- * A 'BufferedOutput' should not be used in multiple threads, there's no locking mechanism to protect
--   buffering state.
--
-- * A 'Output' device should only be used with a single 'BufferedOutput', If multiple 'BufferedOutput' s
--   are opened on a same 'BufferedOutput' device, the output will be interleaved.
--
data BufferedOutput o = BufferedOutput
    { bufOutput     :: o
    , bufIndex      :: {-# UNPACK #-} !Counter
    , outputBuffer  :: {-# UNPACK #-} !(MutablePrimArray RealWorld Word8)
    }

-- | Open a new buffered input with given buffer size, e.g. 'V.defaultChunkSize'.
newBufferedInput :: Int     -- ^ Input buffer size
                 -> i
                 -> IO (BufferedInput i)
newBufferedInput bufSiz i = do
    pb <- newIORef V.empty
    buf <- newPinnedPrimArray (max bufSiz 0)
    inputBuffer <- newIORef buf
    return (BufferedInput i pb inputBuffer)

-- | Open a new buffered output with given buffer size, e.g. 'V.defaultChunkSize'.
newBufferedOutput :: Int    -- ^ Output buffer size
                  -> o
                  -> IO (BufferedOutput o)
newBufferedOutput bufSiz o = do
    index <- newPrimIORef 0
    buf <- newPinnedPrimArray (max bufSiz 0)
    return (BufferedOutput o index buf)

-- | Request bytes from 'BufferedInput'.
--
-- The buffering logic is quite simple:
--
-- If we have pushed back bytes, directly return it, otherwise we read using buffer size.
-- If we read N bytes, and N is larger than half of the buffer size, then we freeze buffer and return,
-- otherwise we copy buffer into result and reuse buffer afterward.
--
readBuffer :: (Input i, HasCallStack) => BufferedInput i -> IO V.Bytes
readBuffer BufferedInput{..} = do
    pb <- readIORef bufPushBack
    if V.null pb
    then do
        rbuf <- readIORef inputBuffer
        bufSiz <- getSizeofMutablePrimArray rbuf
        l <- readInput bufInput (mutablePrimArrayContents rbuf) bufSiz
        if l < bufSiz `quot` 2                -- read less than half size
        then do
            mba <- newPrimArray l              -- copy result into new array
            copyMutablePrimArray mba 0 rbuf 0 l
            ba <- unsafeFreezePrimArray mba
            return $! V.fromArr ba 0 l
        else do                                -- freeze buf into result
            when (bufSiz /= 0) $ do
                buf' <- newPinnedPrimArray bufSiz
                writeIORef inputBuffer buf'
            ba <- unsafeFreezePrimArray rbuf
            return $! V.fromArr ba 0 l
    else do
        writeIORef bufPushBack V.empty
        return pb

-- | Read exactly N bytes
--
-- If EOF reached before N bytes read, trailing bytes will be returned.
--
readExactly :: (HasCallStack, Input i) => Int -> BufferedInput i -> IO V.Bytes
readExactly n0 h0 = V.concat `fmap` (go h0 n0)
  where
    go h n = do
        chunk <- readBuffer h
        let l = V.length chunk
        if l > n
        then do
            let (lastChunk, rest) = V.splitAt n chunk
            unReadBuffer rest h
            return [lastChunk]
        else if l == n
            then return [chunk]
            else if l == 0
                then return [chunk]
                else do
                    chunks <- go h (n - l)
                    return (chunk : chunks)

-- | Read exactly N bytes
--
-- If EOF reached before N bytes read, a 'ShortReadException' will be thrown
--
readExactly' :: (HasCallStack, Input i) => Int -> BufferedInput i -> IO V.Bytes
readExactly' n h = do
    v <- readExactly n h
    if (V.length v /= n)
    then throwIO (ShortReadException callStack)
    else return v

-- | Read all chunks from a 'BufferedInput'.
--
-- This function will loop read until meet EOF('Input' device return 'V.empty'),
-- Useful for reading small file into memory.
readAll :: (HasCallStack, Input i) => BufferedInput i -> IO [V.Bytes]
readAll h = loop []
  where
    loop acc = do
        chunk <- readBuffer h
        if V.null chunk
        then return $! reverse (chunk:acc)
        else loop (chunk:acc)

-- | Read all chunks from a 'BufferedInput', and concat chunks together.
--
-- This function will loop read until meet EOF('Input' device return 'V.empty'),
-- Useful for reading small file into memory.
readAll' :: (HasCallStack, Input i) => BufferedInput i -> IO V.Bytes
readAll' i = V.concat <$> readAll i

data BufferedException = ParseException P.ParseError CallStack
                       | ShortReadException CallStack deriving (Show, Typeable)

instance Exception BufferedException where
    toException = ioExceptionToException
    fromException = ioExceptionFromException

-- | Push bytes back into buffer(if not empty).
--
unReadBuffer :: (HasCallStack, Input i) => V.Bytes -> BufferedInput i -> IO ()
unReadBuffer pb' BufferedInput{..} = unless (V.null pb') $ do
    modifyIORef' bufPushBack $ \ pb -> pb' `V.append` pb

-- | Read buffer and parse with 'Parser'.
--
-- This function will continuously draw data from input before parsing finish. Unconsumed
-- bytes will be returned to buffer.
--
-- Either during parsing or before parsing, reach EOF will result in 'P.ParseError'.
readParser :: (HasCallStack, Input i) => P.Parser a -> BufferedInput i -> IO (Either P.ParseError a)
readParser p i@BufferedInput{..} = do
    bs <- readBuffer i
    (rest, r) <- P.parseChunks p (readBuffer i) bs
    unReadBuffer rest i
    return r

-- | Read until reach a magic bytes
--
-- If EOF is reached before meet a magic byte, partial bytes are returned.
readToMagic :: (HasCallStack, Input i) => Word8 -> BufferedInput i -> IO V.Bytes
readToMagic magic0 h0 = V.concat `fmap` (go h0 magic0)
  where
    go h magic = do
        chunk <- readBuffer h
        if V.null chunk
        then return []
        else case V.elemIndex magic chunk of
            Just i -> do
                let (lastChunk, rest) = V.splitAt (i+1) chunk
                unReadBuffer rest h
                return [lastChunk]
            Nothing -> do
                chunks <- go h magic
                return (chunk : chunks)

-- | Read until reach a magic bytes
--
-- If EOF is reached before meet a magic byte, a 'ShortReadException' will be thrown.
readToMagic' :: (HasCallStack, Input i) => Word8 -> BufferedInput i -> IO V.Bytes
readToMagic' magic0 h0 = V.concat `fmap` (go h0 magic0)
  where
    go h magic = do
        chunk <- readBuffer h
        if V.null chunk
        then throwIO (ShortReadException callStack)
        else case V.elemIndex magic chunk of
            Just i -> do
                let (lastChunk, rest) = V.splitAt (i+1) chunk
                unReadBuffer rest h
                return [lastChunk]
            Nothing -> do
                chunks <- go h magic
                return (chunk : chunks)

-- | Read to a linefeed ('\n' or '\r\n'), return 'Bytes' before it.
--
-- If EOF is reached before meet a line feed, partial line is returned.
readLine :: (HasCallStack, Input i) => BufferedInput i -> IO V.Bytes
readLine i = do
    bs@(V.PrimVector arr s l) <- readToMagic 10 i
    if l == 0
    then return bs
    else return $ case bs `V.indexMaybe` (l-2) of
        Nothing -> V.PrimVector arr s (l-1)
        Just r | r == 13   -> V.PrimVector arr s (l-2)
               | otherwise -> V.PrimVector arr s (l-1)

-- | Read to a linefeed ('\n' or '\r\n'), return 'Bytes' before it.
--
-- If EOF reached before meet a line feed, a 'ShortReadException' will be thrown.
readLine' :: (HasCallStack, Input i) => BufferedInput i -> IO V.Bytes
readLine' i = do
    bs@(V.PrimVector arr s l) <- readToMagic' 10 i
    if l == 0
    then return bs
    else return $ case bs `V.indexMaybe` (l-2) of
        Nothing -> V.PrimVector arr s (l-1)
        Just r | r == 13   -> V.PrimVector arr s (l-2)
               | otherwise -> V.PrimVector arr s (l-1)

--------------------------------------------------------------------------------

-- | Write 'V.Bytes' into buffered handle.
--
-- * If buffer is empty and bytes are larger than half of buffer, directly write bytes,
--   otherwise copy bytes to buffer.
--
-- * If buffer is not empty, then copy bytes to buffer if it can hold, otherwise
--   write buffer first, then try again.
--
writeBuffer :: (HasCallStack, Output o) => BufferedOutput o -> V.Bytes -> IO ()
writeBuffer o@BufferedOutput{..} v@(V.PrimVector ba s l) = do
    i <- readPrimIORef bufIndex
    bufSiz <- getSizeofMutablePrimArray outputBuffer
    if i /= 0
    then if i + l <= bufSiz
        then do
            -- current buffer can hold it
            copyPrimArray outputBuffer i ba s l   -- copy to buffer
            writePrimIORef bufIndex (i+l)              -- update index
        else do
            -- flush the buffer first
            withMutablePrimArrayContents outputBuffer $ \ ptr -> (writeOutput bufOutput) ptr i
            writePrimIORef bufIndex 0
            -- try write to buffer again
            writeBuffer o v
    else
        if l > bufSiz `unsafeShiftR` 2
        then withPrimVectorSafe v (writeOutput bufOutput)
        else do
            copyPrimArray outputBuffer i ba s l   -- copy to buffer
            writePrimIORef bufIndex l             -- update index


-- | Directly write 'B.Builder' into buffered handle.
--
-- Run 'B.Builder' with buffer if it can hold, write to device when buffer is full.
--
writeBuilder :: (HasCallStack, Output o) => BufferedOutput o -> B.Builder a -> IO ()
writeBuilder BufferedOutput{..} (B.Builder b) = do
    i <- readPrimIORef bufIndex
    originBufSiz <- getSizeofMutablePrimArray outputBuffer
    _ <- primToIO (b (B.OneShotAction action) (lastStep originBufSiz) (B.Buffer outputBuffer i))
    return ()
  where
    action :: V.Bytes -> ST RealWorld ()
    action bytes = ioToPrim (withPrimVectorSafe bytes (writeOutput bufOutput))

    lastStep :: Int -> a -> B.BuildStep RealWorld
    lastStep originBufSiz _ (B.Buffer buf offset)
        | sameMutablePrimArray buf outputBuffer = ioToPrim $ do
            writePrimIORef bufIndex offset   -- record new buffer index
            return []
        | offset >= originBufSiz = ioToPrim $ do
            withMutablePrimArrayContents buf $ \ ptr -> (writeOutput bufOutput) ptr offset
            writePrimIORef bufIndex 0
            return [] -- to match 'BuildStep' return type
        | otherwise = ioToPrim $ do
            copyMutablePrimArray outputBuffer 0 buf 0 offset
            writePrimIORef bufIndex offset
            return [] -- to match 'BuildStep' return type

-- | Flush the buffer into output device(if buffer is not empty).
--
flushBuffer :: (HasCallStack, Output o) => BufferedOutput o -> IO ()
flushBuffer BufferedOutput{..} = do
    i <- readPrimIORef bufIndex
    when (i /= 0) $ do
        withMutablePrimArrayContents outputBuffer $ \ ptr -> (writeOutput bufOutput) ptr i
        writePrimIORef bufIndex 0

--------------------------------------------------------------------------------

-- | Type alias for input stream, 'Nothing' indicate EOF.
type Source a = IO (Maybe a)

-- | Type alias for output stream, contain a write & a flush function.
type Sink a = (a -> IO (), IO ())

-- | Turn a 'BufferedInput' into 'Source', map EOF to Nothing.
--
sourceBuffer :: (HasCallStack, Input i) => BufferedInput i -> Source V.Bytes
{-# INLINABLE sourceBuffer #-}
sourceBuffer i = readBuffer i >>= \ x -> if V.null x then return Nothing
                                                     else return (Just x)

-- | Turn a 'BufferedOutput' into 'Sink'.
--
sinkBuffer :: (HasCallStack, Output o) => BufferedOutput o -> Sink V.Bytes
{-# INLINABLE sinkBuffer #-}
sinkBuffer o = (writeBuffer o, flushBuffer o)

-- | Source a list streamly.
sourceFromList :: [a] -> IO (Source a)
{-# INLINABLE sourceFromList #-}
sourceFromList xs = do
    xsRef <- newIORef xs
    return (popper xsRef)
  where
    popper xsRef = do
        xs <- readIORef xsRef
        case xs of
            (x:xs') -> do
                writeIORef xsRef xs'
                return (Just x)
            _ -> return Nothing

-- | Connect two streams, after first reach EOF, draw element from second.
(>+>) :: Source a -> Source a  -> IO (Source a)
{-# INLINABLE (>+>) #-}
input1 >+> input2 = concatSource [input1, input2]

-- | Read all stream elements to a list.
collectSource :: Source a -> IO [a]
{-# INLINABLE collectSource #-}
collectSource input = loop []
  where
    loop acc = do
        r <- input
        case r of
            Just r' -> loop (r':acc)
            _       -> return $! reverse acc


-- | Read buffer and parse with 'Parser'.
--
-- This function will continuously draw data from input before parsing finish. Unconsumed
-- bytes will be returned to buffer.
--
-- Return 'Nothing' if reach EOF before parsing, throw 'ParseException' if parsing fail.
parseSource :: HasCallStack => P.Parser a -> Source V.Bytes -> IO (Source a)
{-# INLINABLE parseSource #-}
parseSource p source = do
    trailingRef <- newIORef V.empty
    return (go trailingRef)
  where
    go trailingRef = do
        trailing <- readIORef trailingRef
        if V.null trailing
        then do
            bs <- source
            case bs of
                Just bs' -> do
                    (rest, r) <- P.parseChunks p source' bs'
                    writeIORef trailingRef rest
                    case r of Right v -> return (Just v)
                              Left e  -> throwIO (ParseException e callStack)
                _    -> return Nothing
        else do
            (rest, r) <- P.parseChunks p source' trailing
            writeIORef trailingRef rest
            case r of Right v -> return (Just v)
                      Left e  -> throwIO (ParseException e callStack)

    source' = source >>= \ r -> case r of Just r' -> return r'
                                          _      -> return V.empty

-- | Connect list of streams, after one stream reach EOF, draw element from next.
concatSource :: [Source a] -> IO (Source a)
{-# INLINABLE concatSource #-}
concatSource ss = newIORef ss >>= return . loop
  where
    loop ref = do
        ss <- readIORef ref
        case ss of
          []       -> return Nothing
          (input:rest) -> do
              chunk <- input
              case chunk of
                Just _  -> return chunk
                _       -> writeIORef ref rest >> loop ref

-- | Zip two streams into one.
zipSource :: Source a -> Source b -> Source (a,b)
{-# INLINABLE zipSource #-}
zipSource inputA inputB = do
    mA <- inputA
    mB <- inputB
    case mA of Just a -> case mB of Just b -> return (Just (a, b))
                                    _ -> return Nothing
               _ -> return Nothing

-- | Loop read stream and write to output, when input ends flush the output.
--
(>>>>=) :: Source a     -- ^ stream to write
        -> Sink a
        -> IO ()
{-# INLINABLE (>>>>=) #-}
(>>>>=) input (write, flush) = loop
  where
    loop = do
        m <- input
        case m of
            Just x' -> write x' >> loop
            _       -> flush
