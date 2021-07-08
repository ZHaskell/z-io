{-|
Module      : Z.IO.UV.UVStream
Description : IO manager based on libuv
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides 'UVStream' handle type.

-}

module Z.IO.UV.UVStream
  ( -- * uv_stream abstraction
    initUVStream
  , UVStream(..)
  , getUVStreamFD
  , closeUVStream
  , shutdownUVStream
  , helloWorld, echo
  ) where

import           Control.Concurrent
import           Control.Monad
import qualified Z.Data.Text.Print          as T
import           Z.IO.UV.Errno
import           Z.IO.UV.FFI
import           Z.IO.UV.Manager
import           Z.IO.Buffered
import           Z.IO.Exception
import           Z.IO.Resource
import           Data.IORef
import           GHC.Ptr

--------------------------------------------------------------------------------
-- UVStream

-- | A haskell data type wrap an @uv_stream_t@ inside
--
-- 'UVStream' DO NOT provide thread safety! Use 'UVStream' concurrently in multiple
-- threads will lead to undefined behavior.
data UVStream = UVStream
    { uvsHandle  :: {-# UNPACK #-} !(Ptr UVHandle)
    , uvsSlot    :: {-# UNPACK #-} !UVSlot
    , uvsManager :: UVManager
    , uvsClosed  :: {-# UNPACK #-} !(IORef Bool)    -- We have no thread-safe guarantee,
                                                    -- so no need to use atomic read&write
    }

instance Show UVStream where show = T.toString

instance T.Print UVStream where
    {-# INLINABLE toUTF8BuilderP #-}
    toUTF8BuilderP _ (UVStream hdl slot uvm _) = do
        "UVStream{uvsHandle="  >> T.toUTF8Builder hdl
        ",uvsSlot="            >> T.toUTF8Builder slot
        ",uvsManager="         >> T.toUTF8Builder uvm
        T.char7 '}'

-- | Safely lock an uv manager and perform uv_handle initialization.
--
-- Initialization an UV stream usually take two step:
--
--   * allocate an uv_stream struct with proper size
--   * lock a particular uv_loop from a uv manager, and perform custom initialization, such as @uv_tcp_init@.
--
-- And this is what 'initUVStream' do, all you need to do is to provide the manager you want to hook the handle
-- onto(usually the one on the same capability, i.e. the one obtained by 'getUVManager'),
-- and provide a custom initialization function (which should throw an exception if failed).
--
initUVStream :: HasCallStack
             => (Ptr UVLoop -> Ptr UVHandle -> IO ())
             -> UVManager
             -> Resource UVStream
{-# INLINABLE initUVStream #-}
initUVStream f uvm = initResource
    (withUVManager uvm $ \ loop -> do
        hdl <- hs_uv_handle_alloc loop
        slot <- getUVSlot uvm (peekUVHandleData hdl)
        _ <- tryTakeMVar =<< getBlockMVar uvm slot   -- clear the parking spot
        -- this function should be run inside mask, no need to protect
        f loop hdl -- `onException` hs_uv_handle_free hdl
        closed <- newIORef False
        return (UVStream hdl slot uvm closed))
    closeUVStream

-- | Manually close a uv stream.
closeUVStream :: UVStream -> IO ()
{-# INLINABLE closeUVStream #-}
closeUVStream (UVStream hdl _ uvm closed) = withUVManager' uvm $ do
    c <- readIORef closed
    -- hs_uv_handle_close won't return error
    unless c $ writeIORef closed True >> hs_uv_handle_close hdl

-- | Shutdown the outgoing (write) side of a duplex stream. It waits for pending write requests to complete.
--
-- Futher writing will throw 'ResourceVanished'(EPIPE).
shutdownUVStream :: HasCallStack => UVStream -> IO ()
{-# INLINABLE shutdownUVStream #-}
shutdownUVStream (UVStream hdl _ uvm closed) = do
    c <- readIORef closed
    when c throwECLOSED
    m <- withUVManager' uvm $ do
        reqSlot <- getUVSlot uvm (hs_uv_shutdown hdl)
        m <- getBlockMVar uvm reqSlot
        _ <- tryTakeMVar m
        return m
    throwUVIfMinus_  (uninterruptibleMask_ $ takeMVar m)

-- | Get stream fd
getUVStreamFD :: HasCallStack => UVStream -> IO FD
{-# INLINABLE getUVStreamFD #-}
getUVStreamFD (UVStream hdl _ _ closed) = do
    c <- readIORef closed
    when c throwECLOSED
    throwUVIfMinus (hs_uv_fileno hdl)

instance Input UVStream where
    -- readInput :: HasCallStack => UVStream -> Ptr Word8 ->  Int -> IO Int
    {-# INLINABLE readInput  #-}
    readInput (UVStream hdl slot uvm closed) buf len = mask_ $ do
        c <- readIORef closed
        when c throwECLOSED
        -- set up buffer
        pokeBufferTable uvm slot buf len
        m <- getBlockMVar uvm slot
        -- clean up
        _ <- tryTakeMVar m

        throwUVIfMinus_ $ withUVManager' uvm (hs_uv_read_start hdl)
        -- since we are inside mask, this is the only place
        -- async exceptions could possibly kick in, and we should stop reading
        r <- takeMVar m `onException` (do
                -- normally we call 'uv_read_stop' in C read callback
                -- but when exception raise, here's the place to stop
                -- stop a handle twice will be a libuv error, so we don't check result
                _ <- withUVManager' uvm (uv_read_stop hdl)
                void (tryTakeMVar m))

        if  | r > 0  -> return r
            | r == fromIntegral UV_EOF -> return 0
            | r < 0 ->  throwUVIfMinus (return r)
            -- r == 0 should be impossible, since we guard this situation in c side
            | otherwise -> throwUVError UV_UNKNOWN IOEInfo{
                                  ioeName = "UVStream read error"
                                , ioeDescription = "UVStream read should never return 0 before EOF"
                                , ioeCallStack = callStack
                                }

instance Output UVStream where
    -- writeOutput :: HasCallStack => UVStream -> Ptr Word8 -> Int -> IO ()
    {-# INLINABLE writeOutput  #-}
    writeOutput (UVStream hdl _ uvm closed) buf len = mask_ $ do
        c <- readIORef closed
        when c throwECLOSED
        m <- withUVManager' uvm $ do
            reqSlot <- getUVSlot uvm (hs_uv_write hdl buf len)
            m <- getBlockMVar uvm reqSlot
            _ <- tryTakeMVar m
            return m
        -- we can't cancel uv_write_t with current libuv,
        -- otherwise disaster will happen if buffer got collected.
        -- so we have to turn to uninterruptibleMask_'s help.
        -- i.e. writing UVStream is an uninterruptible operation.
        -- OS will guarantee writing TTY and socket will not
        -- hang forever anyway.
        throwUVIfMinus_  (uninterruptibleMask_ $ takeMVar m)
        {- wait for https://github.com/libuv/libuv/pull/2874
        -- attempt blocking write first
        r <- hs_uv_try_write hdl buf len
        if  | r == len -> return ()
            | r < 0 && r /= fromIntegral UV_EAGAIN -> throwUV r
            | otherwise -> do
                m <- withUVManager' uvm $ do
                    reqSlot <- if r > 0
                        then getUVSlot uvm (hs_uv_write hdl (buf `plusPtr` r) (len - r))
                        else getUVSlot uvm (hs_uv_write hdl buf len)
                    m <- getBlockMVar uvm reqSlot
                    _ <- tryTakeMVar m
                    return m
                -- we can't cancel uv_write_t with current libuv,
                -- otherwise disaster will happen if buffer got collected.
                -- so we have to turn to uninterruptibleMask_'s help.
                -- i.e. writing UVStream is an uninterruptible operation.
                -- OS will guarantee writing TTY and socket will not
                -- hang forever anyway.
                throwUVIfMinus_  (uninterruptibleMask_ $ takeMVar m)
        -}

--------------------------------------------------------------------------------

-- | Write "hello world" to a 'UVStream'.
helloWorld :: UVStream -> IO ()
{-# INLINABLE helloWorld #-}
helloWorld uvs = writeOutput uvs (Ptr "hello world"#) 11

-- | Echo whatever received bytes.
echo :: UVStream -> IO ()
{-# INLINABLE echo #-}
echo uvs = do
    i <- newBufferedInput uvs
    o <- newBufferedOutput uvs
    forever $ readBuffer i >>= writeBuffer o >> flushBuffer o
