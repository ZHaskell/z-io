{-|
Module      : Z.IO.Process
Description : Process utilities
Copyright   : (c) Dong Han, 2018~2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides process utilities.

@
import Control.Concurrent.MVar
import Z.IO.Process
@

-}

module Z.IO.Process (
    initProcess
  , readProcess 
  , ProcessOptions(..)
  , defaultProcessOptions
  , ProcessStdStream(..)
  , ProcessState(..)
  , waitProcessExit
  , getProcessPID
  , killPID
  -- * internal 
  , spawn
  -- * Constant
  -- ** Signal
  , pattern SIGTERM 
  , pattern SIGINT 
  , pattern SIGQUIT
  , pattern SIGKILL
  , pattern SIGHUP 
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Int
import Data.Primitive.ByteArray
import Foreign.C.Types
import GHC.Generics
import GHC.Conc.Signal (Signal)
import System.Exit
import Z.Data.CBytes
import Z.Data.CBytes        as CBytes
import Z.Data.JSON         (EncodeJSON, ToValue, FromValue)
import Z.Data.Vector        as V
import Z.Data.Text          as T
import Z.Data.Text.ShowT    (ShowT)
import qualified Data.List  as List
import Z.Data.Array.Unaligned
import Z.Foreign
import Z.IO.Buffered
import Z.IO.Exception
import Z.IO.Network.IPC
import Z.IO.Resource
import Z.IO.UV.FFI
import Z.IO.UV.Manager
import Z.IO.UV.UVStream

#include "uv.h"

defaultProcessOptions :: ProcessOptions
defaultProcessOptions = ProcessOptions
    { processFile = "./main"
    , processArgs = []
    , processEnv = Nothing
    , processCWD = "."
    , processFlags = 0
    , processUID = UID 0
    , processGID = GID 0 
    , processStdStreams = (ProcessIgnore, ProcessIgnore, ProcessIgnore)
    }

-- | Process state
data ProcessState = ProcessRunning PID | ProcessExited ExitCode
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (ShowT, EncodeJSON, ToValue, FromValue)

-- | Wait until process exit and return the 'ExitCode'.
waitProcessExit :: TVar ProcessState -> IO ExitCode
waitProcessExit svar = atomically $ do
    s <- readTVar svar
    case s of ProcessExited e -> return e
              _ -> retry

-- | Get process 'PID' if process is running.
getProcessPID :: TVar ProcessState -> IO (Maybe PID)
getProcessPID svar = atomically $ do
    s <- readTVar svar
    case s of ProcessRunning pid -> return (Just pid)
              _ -> return Nothing

-- | Send signals to process.
killPID :: PID -> Signal -> IO ()
killPID (PID pid) sig = throwUVIfMinus_ (uv_kill pid sig)

pattern SIGTERM :: Signal
pattern SIGINT  :: Signal
pattern SIGQUIT :: Signal
pattern SIGKILL :: Signal
pattern SIGHUP  :: Signal
pattern SIGTERM = #const SIGTERM
pattern SIGINT  = #const SIGINT
pattern SIGQUIT = #const SIGQUIT
pattern SIGKILL = #const SIGKILL
pattern SIGHUP  = #const SIGHUP

-- | Resource spawn processes.
--
-- Return a resource spawn processes, when initiated return the @(stdin, stdout, stderr, pstate)@ tuple, 
-- and fork a seperated thread waiting process exit, i.e. the @pstate@ will be updated to `ProcessExited`
-- automatically when the process exits.
-- 
-- A cleanup thread will be started when you finish using the process resource, to close any std stream 
-- created during spawn, i.e. 'ProcessCreate' option are passed.
--
-- @
--    initProcess defaultProcessOptions{processFile="your program" ...} $ (stdin, stdout, stderr, pstate) -> do
--      ... -- read or write from child process's std stream
--      waitProcessExit pstate  -- if you want to wait for process exit on current thread.
-- @
initProcess :: ProcessOptions -> Resource (Maybe UVStream, Maybe UVStream, Maybe UVStream, TVar ProcessState)
initProcess opt = initResource (spawn opt) $ \ (s0,s1,s2, pstate) -> void . forkIO $ do
    waitProcessExit pstate
    forM_ s0 closeUVStream
    forM_ s1 closeUVStream
    forM_ s2 closeUVStream

-- | Spawn a processe with given UTF8 textual input.
--
-- Result output are collected as UTF8 bytes, return with exit code.
readProcess :: ProcessOptions                   -- ^ processStdStreams options are ignored
            -> T.Text                           -- ^ stdin
            -> IO (T.Text, T.Text, ExitCode)    -- ^ stdout, stderr, exit code
readProcess opts inp = do
    withResource (initProcess opts{processStdStreams=(ProcessCreate, ProcessCreate, ProcessCreate)}) 
        $ \ (Just s0, Just s1, Just s2, pstate) -> do
            r1 <- newEmptyMVar 
            r2 <- newEmptyMVar 
            forkIO $ do
                withPrimVectorSafe (T.getUTF8Bytes inp) (writeOutput s0)
                closeUVStream s0
            forkIO $ do
                b1 <- newBufferedInput s1
                readAll' b1 >>= putMVar r1 . T.validate
            forkIO $ do
                b2 <- newBufferedInput s2
                readAll' b2 >>= putMVar r2 . T.validate
            (,,) <$> takeMVar r1 <*> takeMVar r2 <*> waitProcessExit pstate


-- | Spawn a new thread
--
-- Please manually close child process's std stream(if there'are any created) after process exists.
spawn :: ProcessOptions -> IO (Maybe UVStream, Maybe UVStream, Maybe UVStream, TVar ProcessState)
spawn ProcessOptions{..} = do

    (MutableByteArray popts) <- newByteArray (#size uv_process_options_t)
    (MutableByteArray pstdio) <- newByteArray ((#size uv_stdio_container_t)*3)

    pokeMBA popts (#offset uv_process_options_t, flags) processFlags
    pokeMBA popts (#offset uv_process_options_t, uid) processUID
    pokeMBA popts (#offset uv_process_options_t, gid) processGID

    uvm <- getUVManager 

    let (s0, s1, s2) = processStdStreams

    pokeMBA pstdio (#offset uv_stdio_container_t, flags) (processStdStreamFlag s0)
    uvs0' <- case s0 of
        ProcessInherit fd -> do
            pokeMBA pstdio (#offset uv_stdio_container_t, data) fd
            return Nothing
        ProcessCreate -> do
            (uvs0, _) <- acquire (initIPCStream uvm)
            pokeMBA pstdio (#offset uv_stdio_container_t, data) (uvsHandle uvs0)
            return (Just uvs0)
        _ -> return Nothing
        
    pokeMBA pstdio ((#offset uv_stdio_container_t, flags)+(#size uv_stdio_container_t))
                (processStdStreamFlag s1)
    uvs1' <- case s1 of
        ProcessInherit fd -> do
            pokeMBA pstdio ((#offset uv_stdio_container_t, data)+(#size uv_stdio_container_t)) fd
            return Nothing
        ProcessCreate -> do
            (uvs1, _) <- acquire (initIPCStream uvm)
            pokeMBA pstdio ((#offset uv_stdio_container_t, data)+(#size uv_stdio_container_t))
                (uvsHandle uvs1)
            return (Just uvs1)
        _ -> return Nothing

    pokeMBA pstdio ((#offset uv_stdio_container_t, flags)+(#size uv_stdio_container_t)*2)
                (processStdStreamFlag s2)
    uvs2' <- case s2 of
        ProcessInherit fd -> do
            pokeMBA pstdio ((#offset uv_stdio_container_t, data)+(#size uv_stdio_container_t)*2) fd
            return Nothing
        ProcessCreate -> do
            (uvs2, _) <- acquire (initIPCStream uvm)
            pokeMBA pstdio ((#offset uv_stdio_container_t, data.stream)+(#size uv_stdio_container_t)*2)
                (uvsHandle uvs2)
            return (Just uvs2)
        _ -> return Nothing

    -- concat args
    let allArgs = V.intercalateElem 0 (List.map CBytes.toPrimArray processArgs ++ [mempty])  
        argsLen = fromIntegral $ List.length processArgs
    -- concat env
        mkEnv (k, v) = CBytes.toPrimArray (k <> "=" <> v)
        allEnv = case processEnv of
            Just e -> V.intercalateElem 0 (List.map mkEnv e ++ [mempty])  
            _ -> V.singleton 0 
        envLen = case processEnv of
            Just e -> fromIntegral $ List.length e
            _ -> -1

    (slot, pid) <- withCBytesUnsafe processFile $ \ pfile ->
        withCBytesUnsafe processCWD $ \ pcwd ->
            withPrimArrayUnsafe allArgs $ \ pargs _ ->
                withPrimArrayUnsafe allEnv $ \ penv _ ->
                    withUVManager uvm $ \ loop -> do
                        slot <- throwUVIfMinus (hs_uv_spawn loop popts pfile
                                        pargs argsLen penv envLen pcwd pstdio)
                        pid <- peekBufferTable uvm slot
                        return (slot, pid)

    exitLock <- getBlockMVar uvm slot
    ps <- newTVarIO (ProcessRunning (PID (fromIntegral pid)))

    _ <- forkFinally (takeMVar exitLock) $ \ r -> do
        case r of 
            Left _ -> atomically (writeTVar ps (ProcessExited (ExitFailure (-1))))
            Right e -> 
                let e' = if e == 0 then ExitSuccess else ExitFailure e
                in atomically (writeTVar ps (ProcessExited e'))


    return (uvs0', uvs1', uvs2', ps)
