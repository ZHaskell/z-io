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
  , UVStream
  , defaultTCPClientConfig
  , initTCPClient
  , getTCPSockName
  -- * TCP Server
  , TCPServerConfig(..)
  , defaultTCPServerConfig
  , startTCPServer
  , getTCPPeerName
  -- * For test
  , helloWorld
  , echo
  -- * Internal helper
  , setTCPNoDelay
  , setTCPKeepAlive
  , initTCPStream
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Primitive.PrimArray
import           Foreign.Ptr
import           Z.IO.Exception
import           Z.IO.Network.SocketAddr
import           Z.IO.Resource
import           Z.IO.UV.FFI
import           Z.IO.UV.Manager
import           Z.IO.UV.UVStream
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
    liftIO $ do
        forM_ tcpClientAddr $ \ tcpClientAddr' ->
            withSocketAddrUnsafe tcpClientAddr' $ \ localPtr ->
                -- bind is safe without withUVManager
                throwUVIfMinus_ (uv_tcp_bind hdl localPtr 0)
        -- nodelay is safe without withUVManager
        when tcpClientNoDelay . throwUVIfMinus_ $ uv_tcp_nodelay hdl 1
        when (tcpClientKeepAlive > 0) . throwUVIfMinus_ $
            uv_tcp_keepalive hdl 1 tcpClientKeepAlive
        withSocketAddrUnsafe tcpRemoteAddr $ \ targetPtr -> do
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
    }

-- | A default hello world server on localhost:8888
--
-- Test it with @main = startTCPServer defaultTCPServerConfig helloWorldWorker@ or
-- @main = startTCPServer defaultTCPServerConfig echoWorker@, now try @nc -v 127.0.0.1 8888@
--
defaultTCPServerConfig :: TCPServerConfig
defaultTCPServerConfig = TCPServerConfig
    (SocketAddrInet 8888 inetAny)
    128
    True
    30

-- | Start a server
--
-- Fork new worker thread upon a new connection.
--
startTCPServer :: HasCallStack
               => TCPServerConfig
               -> (UVStream -> IO ())   -- ^ worker which will get an accepted TCP stream and
                                        -- run in a seperated haskell thread,
                                        -- will be closed upon exception or worker finishes.
               -> IO ()
startTCPServer TCPServerConfig{..} tcpServerWorker = do
    let backLog = max tcpListenBacklog 128
    serverUVManager <- getUVManager
    withResource (initTCPStream serverUVManager) $ \ (UVStream serverHandle serverSlot _ _) -> do
        -- bind is safe without withUVManager
        withSocketAddrUnsafe tcpListenAddr $ \ addrPtr -> do
            throwUVIfMinus_ (uv_tcp_bind serverHandle addrPtr 0)
        bracket
            (do check <- throwOOMIfNull $ hs_uv_accept_check_alloc
                throwUVIfMinus_ (hs_uv_accept_check_init check serverHandle)
                return check)
            hs_uv_accept_check_close $
            \ check -> do
-- The buffer passing of accept is a litte complicated here, to get maximum performance,
-- we do batch accepting. i.e. recv multiple client inside libuv's event loop:
--
-- we poke uvmanager's buffer table as a Ptr Word8, with byte size (backLog*sizeof(FD))
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
-- we allocate a buffer to hold accepted FDs, pass it just like a normal reading buffer.
-- then we can start listening.
                acceptBuf <- newPinnedPrimArray backLog
                -- https://stackoverflow.com/questions/1953639/is-it-safe-to-cast-socket-to-int-under-win64
                -- FD is 32bit CInt, it's large enough to hold uv_os_sock_t
                let acceptBufPtr = coerce (mutablePrimArrayContents acceptBuf :: Ptr FD)

                withUVManager' serverUVManager $ do
                    -- We use buffersize as accepted fd count(count backwards)
                    pokeBufferTable serverUVManager serverSlot acceptBufPtr (backLog-1)
                    throwUVIfMinus_ (hs_uv_listen serverHandle (fromIntegral backLog))
-- Step 2.
-- we start a uv_check_t for given uv_stream_t, with predefined checking callback
-- see hs_accept_check_cb in hs_uv_stream.c
                    throwUVIfMinus_ $ hs_uv_accept_check_start check

                m <- getBlockMVar serverUVManager serverSlot
                forever $ do
                    -- wait until accept some FDs
                    _ <- takeMVar m
-- Step 3.
-- After uv loop finishes, if we got some FDs, copy the FD buffer, fetch accepted FDs and fork worker threads.

                    -- we lock uv manager here in case of next uv_run overwrite current accept buffer
                    acceptBufCopy <- withUVManager' serverUVManager $ do
                        _ <- tryTakeMVar m
                        acceptCountDown <- peekBufferTable serverUVManager serverSlot
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
                                -- safe without withUVManager
                                when tcpServerWorkerNoDelay . throwUVIfMinus_ $
                                    uv_tcp_nodelay (uvsHandle uvs) 1
                                when (tcpServerWorkerKeepAlive > 0) . throwUVIfMinus_ $
                                    uv_tcp_keepalive (uvsHandle uvs) 1 tcpServerWorkerKeepAlive
                                tcpServerWorker uvs

--------------------------------------------------------------------------------

initTCPStream :: HasCallStack => UVManager -> Resource UVStream
initTCPStream = initUVStream (\ loop hdl -> throwUVIfMinus_ (uv_tcp_init loop hdl))

-- | Enable or disable @TCP_NODELAY@, which enable or disable Nagle’s algorithm.
setTCPNoDelay :: HasCallStack => UVStream -> Bool -> IO ()
setTCPNoDelay uvs nodelay =
    throwUVIfMinus_ (uv_tcp_nodelay (uvsHandle uvs) (if nodelay then 1 else 0))

-- | Enable \/ disable TCP keep-alive. delay is the initial delay in seconds, ignored when enable is zero.
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
    withSocketAddrStorageUnsafe $ \ paddr ->
        void $ withPrimUnsafe (fromIntegral sizeOfSocketAddrStorage :: CInt) $ \ plen ->
            throwUVIfMinus_ (uv_tcp_getsockname (uvsHandle uvs) paddr plen)

-- | Get the address of the peer connected to the handle.
getTCPPeerName :: HasCallStack => UVStream -> IO SocketAddr
getTCPPeerName uvs = do
    withSocketAddrStorageUnsafe $ \ paddr ->
        void $ withPrimUnsafe (fromIntegral sizeOfSocketAddrStorage :: CInt) $ \ plen ->
            throwUVIfMinus_ (uv_tcp_getpeername (uvsHandle uvs) paddr plen)
