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
    spawn
  , ProcessOptions(..)
  , defaultProcessOptions
  , ProcessStdStream(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Int
import Data.Primitive.ByteArray
import Foreign.C.Types
import GHC.Generics
import System.Exit
import Z.Data.CBytes
import Z.Data.CBytes        as CBytes
import Z.Data.JSON         (EncodeJSON, ToValue, FromValue)
import Z.Data.Vector        as V
import Z.Data.Text.ShowT    (ShowT)
import qualified Data.List  as List
import Z.Data.Array.Unaligned
import Z.Foreign
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
    , processStdStreams = (ProcIgnore, ProcIgnore, ProcIgnore)
    }

data ProcessState = ProcRunning PID | ProcExited ExitCode
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (ShowT, EncodeJSON, ToValue, FromValue)

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
        ProcInherit fd -> do
            pokeMBA pstdio (#offset uv_stdio_container_t, data) fd
            return Nothing
        ProcCreate -> do
            (uvs0, _) <- acquire (initIPCStream uvm)
            pokeMBA pstdio (#offset uv_stdio_container_t, data) (uvsHandle uvs0)
            return (Just uvs0)
        _ -> return Nothing
        
    pokeMBA pstdio ((#offset uv_stdio_container_t, flags)+(#size uv_stdio_container_t))
                (processStdStreamFlag s1)
    uvs1' <- case s1 of
        ProcInherit fd -> do
            pokeMBA pstdio ((#offset uv_stdio_container_t, data)+(#size uv_stdio_container_t)) fd
            return Nothing
        ProcCreate -> do
            (uvs1, _) <- acquire (initIPCStream uvm)
            pokeMBA pstdio (#offset uv_stdio_container_t, data) (uvsHandle uvs1)
            return (Just uvs1)
        _ -> return Nothing

    pokeMBA pstdio ((#offset uv_stdio_container_t, flags)+(#size uv_stdio_container_t)*2)
                (processStdStreamFlag s2)
    uvs2' <- case s2 of
        ProcInherit fd -> do
            pokeMBA pstdio ((#offset uv_stdio_container_t, data)+(#size uv_stdio_container_t)*2) fd
            return Nothing
        ProcCreate -> do
            (uvs2, _) <- acquire (initIPCStream uvm)
            pokeMBA pstdio (#offset uv_stdio_container_t, data) (uvsHandle uvs2)
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
    ps <- newTVarIO (ProcRunning (PID (fromIntegral pid)))

    _ <- forkFinally (takeMVar exitLock) $ \ r -> do
        case r of 
            Left _ -> atomically (writeTVar ps (ProcExited (ExitFailure (-1))))
            Right e -> 
                let e' = if e == 0 then ExitSuccess else ExitFailure e
                in atomically (writeTVar ps (ProcExited e'))

        forM_ uvs0' closeUVStream
        forM_ uvs1' closeUVStream
        forM_ uvs2' closeUVStream

    return (uvs0', uvs1', uvs2', ps)
