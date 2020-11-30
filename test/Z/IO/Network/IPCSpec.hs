{-# LANGUAGE OverloadedStrings #-}

module Z.IO.Network.IPCSpec where

import           Control.Concurrent
import           Control.Monad
import           Data.Bits
import           Data.List             as List
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Test.HUnit
import           Test.Hspec
import           Z.Data.Vector         as V
import           Z.Data.Vector.Base    as V
import           Z.IO.Buffered
import           Z.IO.Exception
import           Z.IO.FileSystem       (mkdtemp)
import           Z.IO.Network
import           Z.IO.Resource

spec :: Spec
spec = describe "IPC operations" $ do
    it "roundtrip test" $ do
        let testMsg = V.cycleN 256 "abc"
            longMsg = V.cycleN 2048 "abcdefg"
        tmpDir <- mkdtemp "z-io-test"
        let addr = tmpDir <> "socket-file"

        serverThread <- forkIO $ startIPCServer defaultIPCServerConfig{ ipcListenName = addr } echo

        threadDelay 1000000     -- 1s

        replicateM_ 10 . forkIO $
            withResource (initIPCClient defaultIPCClientConfig{ipcTargetName = addr}) $ \ ipc -> do
                i <- newBufferedInput ipc
                o <- newBufferedOutput ipc

                writeBuffer o testMsg >> flushBuffer o
                testMsg' <- readAll' i
                testMsg' @=? testMsg

                writeBuffer o longMsg >> flushBuffer o
                longMsg' <- readAll' i
                longMsg' @=? longMsg

        threadDelay 5000000     -- 5s
        killThread serverThread
