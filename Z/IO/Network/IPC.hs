{-|
Module      : Z.IO.IPC
Description : Named pipe\/Unix domain servers and clients
Copyright   : (c) Dong Han, 2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides an API for creating IPC servers and clients. IPC Support is implemented with named pipes on Windows, and UNIX domain sockets on other operating systems.

On UNIX, the local domain is also known as the UNIX domain. The path is a filesystem path name. It gets truncated to sizeof(sockaddr_un.sun_path) - 1, which varies on different operating system between 91 and 107 bytes. The typical values are 107 on Linux and 103 on macOS. The path is subject to the same naming conventions and permissions checks as would be done on file creation. It will be visible in the filesystem, and will persist until unlinked.

On Windows, the local domain is implemented using a named pipe. The path must refer to an entry in \\?\pipe\ or \\.\pipe\. Any characters are permitted, but the latter may do some processing of pipe names, such as resolving .. sequences. Despite appearances, the pipe name space is flat. Pipes will not persist, they are removed when the last reference to them is closed.

-}

module Z.IO.Network.IPC (
  -- * IPC Client
    IPCClientConfig(..)
  , UVStream
  , defaultIPCClientConfig
  , initIPCClient
  -- * IPC Server
  , IPCServerConfig(..)
  , defaultIPCServerConfig
  , startIPCServer
  -- * For test
  , helloWorld
  , echo
  -- * Internal helper
  , initIPCStream
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Primitive.PrimArray
import           Foreign.Ptr
import           GHC.Generics
import           Z.Data.CBytes
import           Z.Data.Text.Print   (Print)
import           Z.Data.JSON         (JSON)
import           Z.IO.Exception
import           Z.IO.Resource
import           Z.IO.UV.FFI
import           Z.IO.UV.Manager
import           Z.IO.UV.UVStream
import           Data.Coerce

--------------------------------------------------------------------------------

-- | A IPC client configuration
--
data IPCClientConfig = IPCClientConfig
    { ipcClientName :: Maybe CBytes -- ^ bind to a local file path (Unix) or name (Windows),
                                    -- won't bind if set to 'Nothing'.
    , ipcTargetName :: CBytes       -- ^ target path (Unix) or a name (Windows).
    } deriving (Eq, Ord, Show, Read, Generic)
      deriving anyclass (Print, JSON)

-- | Default config, connect to ".\/ipc".
--
defaultIPCClientConfig :: IPCClientConfig
defaultIPCClientConfig = IPCClientConfig Nothing "./ipc"

-- | init a IPC client 'Resource', which open a new connect when used.
--
initIPCClient :: IPCClientConfig -> Resource UVStream
initIPCClient (IPCClientConfig cname tname) = do
    uvm <- liftIO getUVManager
    client <- initIPCStream uvm
    let hdl = uvsHandle client
    liftIO $ do
        forM_ cname $ \ cname' ->
            withCBytesUnsafe cname' $ \ cname_p ->
                -- bind is safe without withUVManager
                throwUVIfMinus_ (uv_pipe_bind hdl cname_p)
        withCBytesUnsafe tname $ \ tname_p -> do
            void . withUVRequest uvm $ \ _ -> hs_uv_pipe_connect hdl tname_p
    return client

--------------------------------------------------------------------------------

-- | A IPC server configuration
--
data IPCServerConfig = IPCServerConfig
    { ipcListenName       :: CBytes      -- ^ listening path (Unix) or a name (Windows).
    , ipcListenBacklog    :: Int           -- ^ listening pipe's backlog size, should be large enough(>128)
    } deriving (Eq, Ord, Show, Read, Generic)
      deriving anyclass (Print, JSON)

-- | A default hello world server on @.\/ipc@
--
-- Test it with @main = startIPCServer defaultIPCServerConfig@
--
defaultIPCServerConfig :: IPCServerConfig
defaultIPCServerConfig = IPCServerConfig
    "./ipc"
    256

-- | Start a server
--
-- Fork new worker thread upon a new connection.
--
startIPCServer :: HasCallStack
               => IPCServerConfig
               -> (UVStream -> IO ())  -- ^ worker which get an accepted IPC stream,
                                        -- run in a seperated haskell thread,
                                       --  will be closed upon exception or worker finishes.
               -> IO ()
startIPCServer IPCServerConfig{..} ipcServerWorker = do
    let backLog = max ipcListenBacklog 128
    serverUVManager <- getUVManager
    withResource (initIPCStream serverUVManager) $ \ (UVStream serverHandle serverSlot _ _) -> do
        withCBytesUnsafe ipcListenName $ \ name_p -> do
            throwUVIfMinus_ (uv_pipe_bind serverHandle name_p)
        bracket
            (do check <- throwOOMIfNull $ hs_uv_check_alloc
                throwUVIfMinus_ (hs_uv_check_init check serverHandle)
                return check)
            hs_uv_check_close $
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
                        acceptCountDown <- peekBufferSizeTable serverUVManager serverSlot
                        pokeBufferSizeTable serverUVManager serverSlot (backLog-1)

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
                                throwUVIfMinus_ (uv_pipe_init loop hdl 0)
                                throwUVIfMinus_ (uv_pipe_open hdl fd)) uvm) $ \ uvs -> do
                                ipcServerWorker uvs

--------------------------------------------------------------------------------

initIPCStream :: HasCallStack => UVManager -> Resource UVStream
initIPCStream = initUVStream (\ loop hdl ->
    throwUVIfMinus_ (uv_pipe_init loop hdl 0))
