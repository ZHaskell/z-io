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
            addr = SocketAddrInet 12345 inetLoopback

        serverThread <- forkIO $ startTCPServer defaultTCPServerConfig{
                tcpListenAddr = addr
            ,   tcpServerWorker = \ tcp -> do
                    i <- newBufferedInput defaultChunkSize tcp
                    o <- newBufferedOutput defaultChunkSize tcp
                    forever $ readBuffer i >>= writeBuffer o >> flushBuffer o
            }

        threadDelay 100000

        replicateM_ 1000 . forkIO $
            withResource (initTCPClient defaultTCPClientConfig{tcpRemoteAddr = addr}) $ \ tcp -> do
                i <- newBufferedInput defaultChunkSize tcp
                o <- newBufferedOutput defaultChunkSize tcp

                replicateM_ 1000 . forkIO $ do
                    writeBuffer o testMsg >> flushBuffer o
                    testMsg' <- readAll' i
                    testMsg' @=? testMsg

                replicateM_ 1000 . forkIO $ do
                    writeBuffer o longMsg >> flushBuffer o
                    longMsg' <- readAll' i
                    longMsg' @=? longMsg

        killThread serverThread
