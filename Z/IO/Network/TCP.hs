{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Z.IO.Network.TCP
Description : TCP servers and clients
Copyright   : (c) Dong Han, 2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides an API for creating TCP servers and clients.

-}

module Z.IO.Network.TCP (
  -- * TCP Client
    ClientConfig(..)
  , defaultClientConfig
  , initClient
  -- * TCP Server
  , ServerConfig(..)
  , defaultServerConfig
  , startServer
  ) where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Int
import           Data.Primitive.PrimArray
import           Foreign.C.Types
import           Foreign.Ptr
import           GHC.Ptr
import           Z.Foreign
import           Z.Data.Array
import           Z.IO.Buffered
import           Z.IO.Exception
import           Z.IO.Network.SocketAddr
import           Z.IO.Resource
import           Z.IO.UV.FFI
import           Z.IO.UV.Manager
import           Z.Data.Vector
import           Data.Coerce

initTCPStream :: HasCallStack => UVManager -> Resource UVStream
initTCPStream = initUVStream (\ loop handle ->
    throwUVIfMinus_ (uv_tcp_init loop handle))

initTCPExStream :: HasCallStack => CUInt -> UVManager -> Resource UVStream
initTCPExStream family = initUVStream (\ loop handle ->
    throwUVIfMinus_ (uv_tcp_init_ex loop handle family))

--------------------------------------------------------------------------------

-- | A TCP client configuration
--
data ClientConfig = ClientConfig
    { clientLocalAddr :: Maybe SocketAddr -- ^ assign a local address, or let OS pick one
    , clientTargetAddr :: SocketAddr      -- ^ target address
    , clientNoDelay :: Bool             -- ^ if we want to use @TCP_NODELAY@
    }

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig Nothing (SocketAddrInet 8888 inetLoopback) True

initClient :: HasCallStack => ClientConfig -> Resource UVStream
initClient ClientConfig{..} = do
    uvm <- liftIO getUVManager
    client <- initTCPStream uvm
    let handle = uvsHandle client
    liftIO . withSocketAddr clientTargetAddr $ \ targetPtr -> do
        forM_ clientLocalAddr $ \ clientLocalAddr' ->
            withSocketAddr clientLocalAddr' $ \ localPtr ->
                -- bind is safe without withUVManager
                throwUVIfMinus_ (uv_tcp_bind handle localPtr 0)
        -- nodelay is safe without withUVManager
        when clientNoDelay $ throwUVIfMinus_ (uv_tcp_nodelay handle 1)
        withUVRequest uvm $ \ _ -> hs_uv_tcp_connect handle targetPtr
    return client

--------------------------------------------------------------------------------

-- | A TCP server configuration
--
data ServerConfig = ServerConfig
    { serverAddr       :: SocketAddr      -- ^ listening address
    , serverBackLog    :: Int           -- ^ listening socket's backlog size, must be large enough(>128)
    , serverWorker     :: UVStream -> IO ()  -- ^ worker which get an accepted TCP stream,
                                            -- the socket will be closed upon exception or worker finishes.
    , serverWorkerNoDelay :: Bool       -- ^ if we want to use @TCP_NODELAY@
    }

-- | A default hello world server on localhost:8888
--
-- Test it with @main = startServer defaultServerConfig@, now try @nc -v 127.0.0.1 8888@
--
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
    (SocketAddrInet 8888 inetAny)
    256
    (\ uvs -> writeOutput uvs (Ptr "hello world"#) 11)
    True

-- | Start a server
--
-- Fork new worker thread upon a new connection.
--
startServer :: HasCallStack => ServerConfig -> IO ()
startServer ServerConfig{..} = do
    serverUVManager <- getUVManager
    withResource (initTCPStream serverUVManager) $ \ (UVStream serverHandle serverSlot _ _) ->
        bracket
            (throwOOMIfNull $ hs_uv_accept_check_alloc serverHandle)
            hs_uv_accept_check_close $
            \ check -> do
-- The buffer passing of accept is a litte complicated here, to get maximum performance,
-- we do batch accepting. i.e. recv multiple client inside libuv's event loop:
--
-- we poke uvmanager's buffer table as a Ptr Word8, with byte size (serverBackLog*sizeof(UVFD))
-- inside libuv event loop, we cast the buffer back to int32_t* pointer.
-- each accept callback push a new socket fd to the buffer, and increase a counter(buffer_size_table).
-- serverBackLog should be large enough(>128), so under windows we can't possibly filled it up within one
-- uv_run, under unix we hacked uv internal to provide a stop and resume function, when serverBackLog is
-- reached, we will stop receiving.
--
-- once back to haskell side, we read all accepted sockets and fork worker threads.
-- if serverBackLog is reached, we resume receiving from haskell side.
--
-- Step 1.
-- we allocate a new uv_check_t for given uv_stream_t, with predefined checking callback
-- see hs_accept_check_cb in hs_uv_stream.c
                throwUVIfMinus_ $ hs_uv_accept_check_init check
                withSocketAddr serverAddr $ \ addrPtr -> do
                    m <- getBlockMVar serverUVManager serverSlot
                    acceptBuf <- newPinnedPrimArray serverBackLog
                    let acceptBufPtr = coerce (mutablePrimArrayContents acceptBuf :: Ptr UVFD)
-- Step 2.
-- we allocate a buffer to hold accepted FDs, pass it just like a normal reading buffer.
                    withUVManager' serverUVManager $ do
                        -- We use buffersize as accepted fd counter, so we write 0 here
                        pokeBufferTable serverUVManager serverSlot acceptBufPtr (serverBackLog-1)
                        throwUVIfMinus_ (uv_tcp_bind serverHandle addrPtr 0)
                        throwUVIfMinus_ (hs_uv_listen serverHandle (max 4 (fromIntegral serverBackLog)))

                    forever $ do
                        -- cleaning
                        tryTakeMVar m

-- Step 3.
-- Copy buffer, fetch accepted FDs and fork worker threads.

                        -- we lock uv manager here in case of next uv_run overwrite current accept buffer
                        acceptBufCopy <- withUVManager' serverUVManager $ do
                            tryTakeMVar m
                            accepted_down_counter <- peekBufferTable serverUVManager serverSlot
                            -- if accepted_down_counter count to -1, we should resume on haskell side
                            when (accepted_down_counter == -1) (hs_uv_listen_resume serverHandle)
                            -- copy accepted FDs
                            let accepted_count = serverBackLog- accepted_down_counter
                            acceptBuf' <- newPrimArray serverBackLog
                            copyMutablePrimArray acceptBuf' 0 acceptBuf (accepted_down_counter+1) accepted_count
                            pokeBufferTable serverUVManager serverSlot acceptBufPtr (serverBackLog-1)
                            unsafeFreezePrimArray acceptBuf'

                        -- fork worker thread
                        forM_ [0..sizeofPrimArray acceptBufCopy-1] $ \ i -> do
                            let fd = indexPrimArray acceptBufCopy i
                            if fd < 0
                            -- minus fd indicate a server error and we should close server
                            then throwUVIfMinus_ (return fd)
                            -- It's important to use the worker thread's mananger instead of server's one!
                            else void . forkBa $ do
                                uvm <- getUVManager
                                withResource (initUVStream (\ loop handle -> do
                                    throwUVIfMinus_ (uv_tcp_init loop handle)
                                    throwUVIfMinus_ (hs_uv_tcp_open handle fd)) uvm) $ \ client -> do
                                    when serverWorkerNoDelay . throwUVIfMinus_ $
                                        -- safe without withUVManager
                                        uv_tcp_nodelay (uvsHandle client) 1
                                    serverWorker client
