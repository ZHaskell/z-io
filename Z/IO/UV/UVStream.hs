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
  , helloWorld, echo
  ) where

import          Control.Concurrent
import          Control.Monad
import          Z.IO.UV.Errno
import          Z.IO.UV.FFI
import          Z.IO.UV.Manager
import          Z.IO.Buffered
import          Z.IO.Exception
import          Z.IO.Resource
import          Data.IORef
import          GHC.Ptr

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

instance Show UVStream where
    show (UVStream hdl slot uvm _) =
        "UVStream{uvsHandle=" ++ show hdl ++
                ",uvsSlot=" ++ show slot ++
                ",uvsManager=" ++ show uvm ++ "}"

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
initUVStream f uvm = initResource
    (withUVManager uvm $ \ loop -> do
        hdl <- hs_uv_handle_alloc loop
        slot <- getUVSlot uvm (peekUVHandleData hdl)
        _ <- tryTakeMVar =<< getBlockMVar uvm slot   -- clear the parking spot
        f loop hdl `onException` hs_uv_handle_free hdl
        closed <- newIORef False
        return (UVStream hdl slot uvm closed))
    closeUVStream

-- | Manually close a uv stream.
closeUVStream :: UVStream -> IO ()
closeUVStream (UVStream hdl _ uvm closed) = withUVManager' uvm $ do
    c <- readIORef closed
    -- hs_uv_handle_close won't return error
    unless c $ writeIORef closed True >> hs_uv_handle_close hdl

-- | Get stream fd
getUVStreamFD :: HasCallStack => UVStream -> IO FD
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
                throwUVIfMinus_ $ withUVManager' uvm (uv_read_stop hdl)
                void (tryTakeMVar m))

        if  | r > 0  -> return r
            -- r == 0 should be impossible, since we guard this situation in c side
            | r == fromIntegral UV_EOF -> return 0
            | r < 0 ->  throwUVIfMinus (return r)

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

--------------------------------------------------------------------------------

-- | Write "hello world" to a 'UVStream'.
helloWorld :: UVStream -> IO ()
helloWorld uvs = writeOutput uvs (Ptr "hello world"#) 11

-- | Echo whatever received bytes.
echo :: UVStream -> IO ()
echo uvs = do
    i <- newBufferedInput uvs
    o <- newBufferedOutput uvs
    forever $ readBuffer i >>= writeBuffer o >> flushBuffer o
