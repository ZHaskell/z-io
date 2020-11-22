{-# LANGUAGE OverloadedStrings #-}

module Z.IO.Network.TCPSpec where

import           Control.Concurrent
import           Control.Monad
import           Data.Bits
import           Z.Data.Vector         as V
import           Z.Data.Vector.Base    as V
import           Data.List               as List
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Z.IO.Exception
import           Z.IO.Resource
import           Z.IO.Buffered
import           Z.IO.Network
import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = describe "TCP operations" $ do
    it "roundtrip test" $ do
        let testMsg = V.cycleN 256 "abc"
            longMsg = V.cycleN 2048 "abcdefg"
            addr = SocketAddrInet inetLoopback 12345

        serverThread <- forkIO $ startTCPServer defaultTCPServerConfig{ tcpListenAddr = addr } echo

        threadDelay 2000000  -- 2s

        replicateM_  512 . forkIO $
            withResource (initTCPClient defaultTCPClientConfig{tcpRemoteAddr = addr}) $ \ tcp -> do
                i <- newBufferedInput tcp
                o <- newBufferedOutput tcp

                writeBuffer o testMsg >> flushBuffer o
                testMsg' <- readExactly (V.length testMsg) i
                testMsg' @=? testMsg

                writeBuffer o longMsg >> flushBuffer o
                longMsg' <- readExactly (V.length longMsg) i
                longMsg' @=? longMsg

        threadDelay 5000000  -- 5s
        killThread serverThread
