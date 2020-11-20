{-|
Module      : Z.IO.Buffered
Description : Buffered IO interface
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides low level buffered IO interface, it's recommended to check higher level streaming interface
"Z.IO.BIO" first as it provides more features.

-}

module Z.IO.Buffered
  ( -- * Input & Output device
    Input(..), Output(..)
    -- * Buffered Input
  , BufferedInput, bufInput
  , newBufferedInput
  , newBufferedInput'
  , readBuffer, readBufferText
  , unReadBuffer
  , readParser
  , readExactly,  readExactly'
  , readToMagic, readToMagic'
  , readLine, readLine'
  , readAll, readAll'
    -- * Buffered Output
  , BufferedOutput, bufOutput
  , newBufferedOutput
  , newBufferedOutput'
  , writeBuffer
  , writeBuilder
  , flushBuffer
    -- * Exceptions
  , IncompleteInput(..)
    -- * common buffer size
  , V.defaultChunkSize
  , V.smallChunkSize
  , V.chunkOverhead
  ) where

import           Control.Monad
import           Data.IORef
import           Data.Primitive.PrimArray
import           Data.Word
import           Data.Bits                 (unsafeShiftR)
import           Foreign.Ptr
import           Z.Data.Array
import qualified Z.Data.Builder.Base       as B
import qualified Z.Data.Parser             as P
import qualified Z.Data.Vector             as V
import qualified Z.Data.Text               as T
import qualified Z.Data.Text.UTF8Codec     as T
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
data BufferedInput = BufferedInput
    { bufInput    :: HasCallStack => Ptr Word8 -> Int -> IO Int
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
data BufferedOutput = BufferedOutput
    { bufOutput     :: HasCallStack => Ptr Word8 -> Int -> IO ()
    , bufIndex      :: {-# UNPACK #-} !Counter
    , outputBuffer  :: {-# UNPACK #-} !(MutablePrimArray RealWorld Word8)
    }

-- | Open a new buffered input with 'V.defaultChunkSize' as buffer size.
newBufferedInput :: Input i => i -> IO BufferedInput
newBufferedInput = newBufferedInput' V.defaultChunkSize

-- | Open a new buffered output with 'V.defaultChunkSize' as buffer size.
newBufferedOutput :: Output o => o -> IO BufferedOutput
newBufferedOutput = newBufferedOutput' V.defaultChunkSize

-- | Open a new buffered output with given buffer size, e.g. 'V.defaultChunkSize'.
newBufferedOutput' :: Output o
                   => Int    -- ^ Output buffer size
                   -> o
                   -> IO BufferedOutput
newBufferedOutput' bufSiz o = do
    index <- newPrimIORef 0
    buf <- newPinnedPrimArray (max bufSiz 0)
    return (BufferedOutput (writeOutput o) index buf)

-- | Open a new buffered input with given buffer size, e.g. 'V.defaultChunkSize'.
newBufferedInput' :: Input i
                  => Int     -- ^ Input buffer size
                  -> i
                  -> IO BufferedInput
newBufferedInput' bufSiz i = do
    pb <- newIORef V.empty
    buf <- newPinnedPrimArray (max bufSiz 0)
    inputBuffer <- newIORef buf
    return (BufferedInput (readInput i) pb inputBuffer)


-- | Request bytes chunk from 'BufferedInput'.
--
-- The buffering logic is quite simple:
--
-- If we have pushed back bytes, directly return it, otherwise we read using buffer size.
-- If we read N bytes, and N is larger than half of the buffer size, then we freeze buffer and return,
-- otherwise we copy buffer into result and reuse buffer afterward.
--
readBuffer :: HasCallStack => BufferedInput -> IO V.Bytes
readBuffer BufferedInput{..} = do
    pb <- readIORef bufPushBack
    if V.null pb
    then do
        rbuf <- readIORef inputBuffer
        bufSiz <- getSizeofMutablePrimArray rbuf
        l <- bufInput (mutablePrimArrayContents rbuf) bufSiz
        if l < bufSiz `unsafeShiftR` 1                -- read less than half size
        then do
            mba <- newPrimArray l              -- copy result into new array
            copyMutablePrimArray mba 0 rbuf 0 l
            ba <- unsafeFreezePrimArray mba
            return $! V.fromArr ba 0 l
        else do                                -- freeze buf into result
            when (bufSiz /= 0) $ do
                buf' <- newPinnedPrimArray bufSiz
                writeIORef inputBuffer buf'
            shrinkMutablePrimArray rbuf l
            ba <- unsafeFreezePrimArray rbuf
            return $! V.fromArr ba 0 l
    else do
        writeIORef bufPushBack V.empty
        return pb

-- | Request UTF8 'T.Text' chunk from 'BufferedInput'.
--
-- The buffer size must be larger than 4 bytes to guarantee decoding progress.
readBufferText :: HasCallStack => BufferedInput -> IO T.Text
readBufferText BufferedInput{..} = do
    pb <- readIORef bufPushBack
    rbuf <- readIORef inputBuffer
    bufSiz <- getSizeofMutablePrimArray rbuf
    if V.null pb
    then do
        l <- bufInput (mutablePrimArrayContents rbuf) bufSiz
        handleBuf l
    else do
        -- clear push back first
        writeIORef bufPushBack V.empty
        let (arr, s, delta) = V.toArr pb
        if T.decodeCharLen arr s <= delta
        -- trailing bytes still contain text
        then splitLastChar pb
        -- trailing bytes contain partial codepoint
        else do
            -- copy trailing bytes to buffer and read
            copyPrimArray rbuf 0 arr s delta
            l <- bufInput (mutablePrimArrayContents rbuf `plusPtr` delta) (bufSiz - delta)
            -- if EOF is reached, no further progress is possible
            when (l == 0) (throwIO (IncompleteInput callStack))
            handleBuf (l + delta)
  where
    handleBuf l = do
        rbuf <- readIORef inputBuffer
        bufSiz <- getSizeofMutablePrimArray rbuf
        if l < bufSiz `unsafeShiftR` 1                -- read less than half size
        then do
            mba <- newPrimArray l              -- copy result into new array
            copyMutablePrimArray mba 0 rbuf 0 l
            ba <- unsafeFreezePrimArray mba
            splitLastChar (V.PrimVector ba 0 l)
        else do                                -- freeze buf into result
            when (bufSiz /= 0) $ do
                buf' <- newPinnedPrimArray bufSiz
                writeIORef inputBuffer buf'
            shrinkMutablePrimArray rbuf l
            ba <- unsafeFreezePrimArray rbuf
            splitLastChar (V.PrimVector ba 0 l)

    splitLastChar bs@(V.toArr -> (arr, s, l))
        | l == 0 = return T.empty
        | otherwise = do
            let (i, _) = V.findR (\ w -> w >= 0b11000000 || w <= 0b01111111) bs
            if (i == -1)
            then throwIO (T.InvalidUTF8Exception callStack)
            else do
                if T.decodeCharLen arr (s + i) > l - i
                then do
                    writeIORef bufPushBack (V.fromArr arr (s+i) (l-i))
                    return (T.validate (V.fromArr arr s i))
                else return (T.validate bs)

-- | Read N bytes(may be smaller than N if EOF reached).
--
-- If EOF reached before N bytes read, trailing bytes will be returned.
--
readExactly :: HasCallStack => Int -> BufferedInput -> IO V.Bytes
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
-- If EOF reached before N bytes read, a 'IncompleteInput' will be thrown
--
readExactly' :: HasCallStack => Int -> BufferedInput -> IO V.Bytes
readExactly' n h = do
    v <- readExactly n h
    if (V.length v /= n)
    then throwIO (IncompleteInput callStack)
    else return v

-- | Read all chunks from a 'BufferedInput'.
--
-- This function will loop read until meet EOF('Input' device return 'V.empty'),
-- Useful for reading small file into memory.
readAll :: HasCallStack => BufferedInput -> IO [V.Bytes]
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
readAll' :: HasCallStack => BufferedInput -> IO V.Bytes
readAll' i = V.concat <$> readAll i

-- | Exceptions when read not enough input.
--
-- Note this exception is a sub-type of 'SomeIOException'.
data IncompleteInput = IncompleteInput CallStack deriving Show
instance Exception IncompleteInput where
    toException = ioExceptionToException
    fromException = ioExceptionFromException

-- | Push bytes back into buffer(if not empty).
--
unReadBuffer :: HasCallStack => V.Bytes -> BufferedInput -> IO ()
unReadBuffer pb' BufferedInput{..} = unless (V.null pb') $ do
    modifyIORef' bufPushBack (\ pb -> pb' `V.append` pb)

-- | Read buffer and parse with 'Parser'.
--
-- This function will continuously draw data from input before parsing finish. Unconsumed
-- bytes will be returned to buffer.
--
-- Either during parsing or before parsing, reach EOF will result in 'P.ParseError'.
readParser :: HasCallStack => P.Parser a -> BufferedInput -> IO (Either P.ParseError a)
readParser p i = do
    bs <- readBuffer i
    (rest, r) <- P.parseChunks p (readBuffer i) bs
    unReadBuffer rest i
    return r

-- | Read until reach a magic bytes, return bytes(including the magic bytes)
--
-- If EOF is reached before meet a magic byte, partial bytes are returned.
readToMagic :: HasCallStack => Word8 -> BufferedInput -> IO V.Bytes
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

-- | Read until reach a magic bytes, return bytes(including the magic bytes)
--
-- If EOF is reached before meet a magic byte, a 'IncompleteInput' will be thrown.
readToMagic' :: HasCallStack => Word8 -> BufferedInput -> IO V.Bytes
readToMagic' magic0 h0 = V.concat `fmap` (go h0 magic0)
  where
    go h magic = do
        chunk <- readBuffer h
        if V.null chunk
        then throwIO (IncompleteInput callStack)
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
-- Return bytes don't include linefeed, empty bytes indicate empty line, 'Nothing' indicate EOF.
-- If EOF is reached before meet a line feed, partial line is returned.
readLine :: HasCallStack => BufferedInput -> IO (Maybe V.Bytes)
readLine i = do
    bs@(V.PrimVector arr s l) <- readToMagic 10 i
    if l == 0
    then return Nothing
    else return $ case bs `V.indexMaybe` (l-2) of
        Nothing -> Just (V.PrimVector arr s (l-1))
        Just r | r == 13   -> Just (V.PrimVector arr s (l-2))
               | otherwise -> Just (V.PrimVector arr s (l-1))

-- | Read to a linefeed ('\n' or '\r\n'), return 'Bytes' before it.
--
-- Return bytes don't include linefeed, empty bytes indicate empty line, 'Nothing' indicate EOF.
-- If EOF reached before meet a line feed, a 'IncompleteInput' will be thrown.
readLine' :: HasCallStack => BufferedInput -> IO (Maybe V.Bytes)
readLine' i = do
    bs@(V.PrimVector arr s l) <- readToMagic' 10 i
    if l == 0
    then return Nothing
    else return $ case bs `V.indexMaybe` (l-2) of
        Nothing -> Just (V.PrimVector arr s (l-1))
        Just r | r == 13   -> Just (V.PrimVector arr s (l-2))
               | otherwise -> Just (V.PrimVector arr s (l-1))

--------------------------------------------------------------------------------

-- | Write 'V.Bytes' into buffered handle.
--
-- * If buffer is empty and bytes are larger than half of buffer, directly write bytes,
--   otherwise copy bytes to buffer.
--
-- * If buffer is not empty, then copy bytes to buffer if it can hold, otherwise
--   write buffer first, then try again.
--
writeBuffer :: HasCallStack => BufferedOutput -> V.Bytes -> IO ()
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
            withMutablePrimArrayContents outputBuffer $ \ ptr -> bufOutput ptr i
            writePrimIORef bufIndex 0
            -- try write to buffer again
            writeBuffer o v
    else
        if l > bufSiz `unsafeShiftR` 1
        then withPrimVectorSafe v bufOutput
        else do
            copyPrimArray outputBuffer i ba s l   -- copy to buffer
            writePrimIORef bufIndex l             -- update index


-- | Directly write 'B.Builder' into buffered handle.
--
-- Run 'B.Builder' with buffer if it can hold, write to device when buffer is full.
--
writeBuilder :: HasCallStack => BufferedOutput -> B.Builder a -> IO ()
writeBuilder BufferedOutput{..} (B.Builder b) = do
    i <- readPrimIORef bufIndex
    originBufSiz <- getSizeofMutablePrimArray outputBuffer
    loop originBufSiz =<< b (\ _ -> return . B.Done) (B.Buffer outputBuffer i)
  where
    loop originBufSiz r = case r of
        B.Done buffer@(B.Buffer buf' i') -> do
            if sameMutablePrimArray buf' outputBuffer
            then writePrimIORef bufIndex i'
            else if i' >= originBufSiz
                then do
                    action =<< freezeBuffer buffer
                    writePrimIORef bufIndex 0
                else do
                    copyMutablePrimArray outputBuffer 0 buf' 0 i'
                    writePrimIORef bufIndex i'
        B.BufferFull buffer@(B.Buffer _ i') wantSiz k -> do
            when (i' /= 0) (action =<< freezeBuffer buffer)
            if wantSiz <= originBufSiz
            then loop originBufSiz =<< k (B.Buffer outputBuffer 0)
            else do
                tempBuf <- newPinnedPrimArray wantSiz
                loop originBufSiz =<< k (B.Buffer tempBuf 0)
        B.InsertBytes buffer@(B.Buffer _ i')  bs@(V.PrimVector arr s l) k -> do
            when (i' /= 0) (action =<< freezeBuffer buffer)
            if V.length bs < originBufSiz
            then do
                copyPrimArray outputBuffer 0 arr s l
                loop originBufSiz =<< k (B.Buffer outputBuffer l)
            else do
                action bs
                loop originBufSiz =<< k (B.Buffer outputBuffer 0)

    action bytes = withPrimVectorSafe bytes bufOutput

    freezeBuffer (B.Buffer buf offset) = do
        -- we can't shrink buffer here, it will be reused
        -- when (offset < siz) (A.shrinkMutablePrimArray buf offset)
        !arr <- unsafeFreezePrimArray buf
        return (V.PrimVector arr 0 offset)

-- | Flush the buffer into output device(if buffer is not empty).
--
flushBuffer :: HasCallStack => BufferedOutput -> IO ()
flushBuffer BufferedOutput{..} = do
    i <- readPrimIORef bufIndex
    when (i /= 0) $ do
        withMutablePrimArrayContents outputBuffer $ \ ptr -> bufOutput ptr i
        writePrimIORef bufIndex 0

