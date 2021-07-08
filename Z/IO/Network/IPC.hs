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

import           Control.Monad
import           Control.Monad.IO.Class
import           GHC.Generics
import           Z.Data.CBytes
import           Z.Data.Text.Print   (Print)
import           Z.Data.JSON         (JSON)
import           Z.IO.Exception
import           Z.IO.Resource
import           Z.IO.Network.TCP    (startServerLoop)
import           Z.IO.UV.FFI
import           Z.IO.UV.Manager
import           Z.IO.UV.UVStream

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
{-# INLINABLE defaultIPCClientConfig #-}
defaultIPCClientConfig = IPCClientConfig Nothing "./ipc"

-- | init a IPC client 'Resource', which open a new connect when used.
--
initIPCClient :: IPCClientConfig -> Resource UVStream
{-# INLINABLE initIPCClient #-}
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
{-# INLINABLE defaultIPCServerConfig #-}
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
{-# INLINABLE startIPCServer #-}
startIPCServer IPCServerConfig{..} = startServerLoop
    (max ipcListenBacklog 128)
    initIPCStream
    (\ serverHandle ->
        withCBytesUnsafe ipcListenName $ \ name_p -> do
            throwUVIfMinus_ (uv_pipe_bind serverHandle name_p))
    ( \ fd worker -> void . forkBa $ do
        uvm <- getUVManager
        withResource (initUVStream (\ loop hdl -> do
            throwUVIfMinus_ (uv_pipe_init loop hdl 0)
            throwUVIfMinus_ (uv_pipe_open hdl fd)) uvm) $ \ uvs -> do
            worker uvs)

--------------------------------------------------------------------------------

initIPCStream :: HasCallStack => UVManager -> Resource UVStream
{-# INLINABLE initIPCStream #-}
initIPCStream = initUVStream (\ loop hdl ->
    throwUVIfMinus_ (uv_pipe_init loop hdl 0))
