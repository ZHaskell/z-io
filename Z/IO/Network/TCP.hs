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
    TCPClientConfig(..)
  , defaultTCPClientConfig
  , initTCPClient
  , getTCPSockName
  -- * TCP Server
  , TCPServerConfig(..)
  , defaultTCPServerConfig
  , startTCPServer
  , getTCPPeerName
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Primitive.PrimArray
import           Foreign.Ptr
import           Foreign.C
import           GHC.Ptr
import           Z.IO.Buffered
import           Z.IO.Exception
import           Z.IO.Network.SocketAddr
import           Z.IO.Resource
import           Z.IO.UV.FFI
import           Z.IO.UV.Manager
import           Z.Foreign
import           Data.Coerce

--------------------------------------------------------------------------------

-- | A TCP client configuration
--
data TCPClientConfig = TCPClientConfig
    { tcpClientAddr :: Maybe SocketAddr -- ^ assign a local address, or let OS pick one
    , tcpRemoteAddr :: SocketAddr       -- ^ remote target address
    , tcpClientNoDelay :: Bool          -- ^ if we want to use @TCP_NODELAY@
    , tcpClientKeepAlive :: CUInt       -- ^ set keepalive delay for client socket, see 'setTCPKeepAlive'
    }

-- | Default config, connect to @localhost:8888@.
--
defaultTCPClientConfig :: TCPClientConfig
defaultTCPClientConfig = TCPClientConfig Nothing (SocketAddrInet 8888 inetLoopback) True 30

-- | init a TCP client 'Resource', which open a new connect when used.
--
initTCPClient :: HasCallStack => TCPClientConfig -> Resource UVStream
initTCPClient TCPClientConfig{..} = do
    uvm <- liftIO getUVManager
    client <- initTCPStream uvm
    let hdl = uvsHandle client
    liftIO . withSocketAddr tcpRemoteAddr $ \ targetPtr -> do
        forM_ tcpClientAddr $ \ tcpClientAddr' ->
            withSocketAddr tcpClientAddr' $ \ localPtr ->
                -- bind is safe without withUVManager
                throwUVIfMinus_ (uv_tcp_bind hdl localPtr 0)
        -- nodelay is safe without withUVManager
        when tcpClientNoDelay . throwUVIfMinus_ $ uv_tcp_nodelay hdl 1
        when (tcpClientKeepAlive > 0) . throwUVIfMinus_ $
            uv_tcp_keepalive hdl 1 tcpClientKeepAlive
        void . withUVRequest uvm $ \ _ -> hs_uv_tcp_connect hdl targetPtr
    return client

--------------------------------------------------------------------------------

-- | A TCP server configuration
--
data TCPServerConfig = TCPServerConfig
    { tcpListenAddr       :: SocketAddr      -- ^ listening address
    , tcpListenBacklog    :: Int           -- ^ listening socket's backlog size, should be large enough(>128)
    , tcpServerWorkerNoDelay :: Bool       -- ^ if we want to use @TCP_NODELAY@
    , tcpServerWorkerKeepAlive :: CUInt    -- ^ set keepalive delay for worker socket, see 'setTCPKeepAlive'
    , tcpServerWorker     :: UVStream -> IO ()  -- ^ worker which get an accepted TCP stream,
                                            -- the socket will be closed upon exception or worker finishes.
    }

-- | A default hello world server on localhost:8888
--
-- Test it with @main = startTCPServer defaultTCPServerConfig@, now try @nc -v 127.0.0.1 8888@
--
defaultTCPServerConfig :: TCPServerConfig
defaultTCPServerConfig = TCPServerConfig
    (SocketAddrInet 8888 inetAny)
    256
    True
    30
    (\ uvs -> writeOutput uvs (Ptr "hello world"#) 11)

-- | Start a server
--
-- Fork new worker thread upon a new connection.
--
startTCPServer :: HasCallStack => TCPServerConfig -> IO ()
startTCPServer TCPServerConfig{..} = do
    let backLog = max tcpListenBacklog 128
    serverUVManager <- getUVManager
    withResource (initTCPStream serverUVManager) $ \ (UVStream serverHandle serverSlot _ _) ->
        bracket
            (throwOOMIfNull $ hs_uv_accept_check_alloc serverHandle)
            hs_uv_accept_check_close $
            \ check -> do
-- The buffer passing of accept is a litte complicated here, to get maximum performance,
-- we do batch accepting. i.e. recv multiple client inside libuv's event loop:
--
-- we poke uvmanager's buffer table as a Ptr Word8, with byte size (backLog*sizeof(UVFD))
-- inside libuv event loop, we cast the buffer back to int32_t* pointer.
-- each accept callback push a new socket fd to the buffer, and increase a counter(buffer_size_table).
-- backLog should be large enough(>128), so under windows we can't possibly filled it up within one
-- uv_run, under unix we hacked uv internal to provide a stop and resume function, when backLog is
-- reached, we will stop receiving.
--
-- once back to haskell side, we read all accepted sockets and fork worker threads.
-- if backLog is reached, we resume receiving from haskell side.
--
-- Step 1.
-- we allocate a new uv_check_t for given uv_stream_t, with predefined checking callback
-- see hs_accept_check_cb in hs_uv_stream.c
                throwUVIfMinus_ $ hs_uv_accept_check_init check
                withSocketAddr tcpListenAddr $ \ addrPtr -> do
                    m <- getBlockMVar serverUVManager serverSlot
                    acceptBuf <- newPinnedPrimArray backLog
                    let acceptBufPtr = coerce (mutablePrimArrayContents acceptBuf :: Ptr UVFD)
-- Step 2.
-- we allocate a buffer to hold accepted FDs, pass it just like a normal reading buffer.
                    withUVManager' serverUVManager $ do
                        -- We use buffersize as accepted fd count(count backwards)
                        pokeBufferTable serverUVManager serverSlot acceptBufPtr (backLog-1)
                        throwUVIfMinus_ (uv_tcp_bind serverHandle addrPtr 0)
                        throwUVIfMinus_ (hs_uv_listen serverHandle (max 4 (fromIntegral backLog)))

                    forever $ do
                        -- wait until accept some FDs
                        !acceptCountDown <- takeMVar m
-- Step 3.
-- Copy buffer, fetch accepted FDs and fork worker threads.

                        -- we lock uv manager here in case of next uv_run overwrite current accept buffer
                        acceptBufCopy <- withUVManager' serverUVManager $ do
                            _ <- tryTakeMVar m
                            pokeBufferTable serverUVManager serverSlot acceptBufPtr (backLog-1)

                            -- if acceptCountDown count to -1, we should resume on haskell side
                            when (acceptCountDown == -1) (hs_uv_listen_resume serverHandle)

                            -- copy accepted FDs
                            let acceptCount = backLog - 1 - acceptCountDown
                            acceptBuf' <- newPrimArray acceptCount
                            copyMutablePrimArray acceptBuf' 0 acceptBuf (acceptCountDown+1) acceptCount
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
                                withResource (initUVStream (\ loop hdl -> do
                                    throwUVIfMinus_ (uv_tcp_init loop hdl)
                                    throwUVIfMinus_ (hs_uv_tcp_open hdl fd)) uvm) $ \ uvs -> do
                                    when tcpServerWorkerNoDelay . throwUVIfMinus_ $
                                        -- safe without withUVManager
                                        uv_tcp_nodelay (uvsHandle uvs) 1
                                    when (tcpServerWorkerKeepAlive > 0) . throwUVIfMinus_ $
                                        uv_tcp_keepalive (uvsHandle uvs) 1 tcpServerWorkerKeepAlive
                                    tcpServerWorker uvs

--------------------------------------------------------------------------------

initTCPStream :: HasCallStack => UVManager -> Resource UVStream
initTCPStream = initUVStream (\ loop hdl -> throwUVIfMinus_ (uv_tcp_init loop hdl))

-- | Enable or disable @TCP_NODELAY@, which disables Nagle’s algorithm.
setTCPNoDelay :: HasCallStack => UVStream -> Bool -> IO ()
setTCPNoDelay uvs nodelay =
    throwUVIfMinus_ (uv_tcp_nodelay (uvsHandle uvs) (if nodelay then 1 else 0))

-- Enable / disable TCP keep-alive. delay is the initial delay in seconds, ignored when enable is zero.
--
-- After delay has been reached, 10 successive probes, each spaced 1 second from the previous one,
-- will still happen. If the connection is still lost at the end of this procedure,
-- then the connection is closed, pending io thread will throw 'TimeExpired' exception.
setTCPKeepAlive :: HasCallStack => UVStream -> CUInt -> IO ()
setTCPKeepAlive uvs delay
    | delay > 0 = throwUVIfMinus_ (uv_tcp_keepalive (uvsHandle uvs) 1 delay)
    | otherwise = throwUVIfMinus_ (uv_tcp_keepalive (uvsHandle uvs) 0 0)

-- | Get the current address to which the handle is bound.
getTCPSockName :: HasCallStack => UVStream -> IO SocketAddr
getTCPSockName uvs = do
    withSocketAddrStorage $ \ paddr ->
        void $ withPrimUnsafe (fromIntegral sizeOfSocketAddrStorage :: CInt) $ \ plen ->
            throwUVIfMinus_ (uv_udp_getsockname (uvsHandle uvs) paddr plen)

-- | Get the address of the peer connected to the handle.
getTCPPeerName :: HasCallStack => UVStream -> IO SocketAddr
getTCPPeerName uvs = do
    withSocketAddrStorage $ \ paddr ->
        void $ withPrimUnsafe (fromIntegral sizeOfSocketAddrStorage :: CInt) $ \ plen ->
            throwUVIfMinus_ (uv_udp_getpeername (uvsHandle uvs) paddr plen)
