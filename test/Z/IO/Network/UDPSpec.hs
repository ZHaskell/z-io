{-# LANGUAGE OverloadedStrings #-}

module Z.IO.Network.UDPSpec where

import           Control.Concurrent
import           Control.Monad
import           Data.Bits
import           Data.IORef
import           Z.Data.Vector         as V
import           Z.Data.Vector.Base    as V
import           Data.List               as List
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Z.IO.Exception
import           Z.IO.Resource
import           Z.IO.Network
import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = describe "UDP operations" $ do
    it "roundtrip test" $ do
        let testMsg = V.replicate 256 48
            longMsg = V.replicate 2048 48
            addr = SocketAddrIPv4 ipv4Loopback 12345
        withResource (initUDP defaultUDPConfig{udpSendMsgSize = 2048}) $ \ c ->
            withResource (initUDP defaultUDPConfig{udpLocalAddr = Just (addr,UDP_DEFAULT)}) $ \ s -> do
                forkIO $ sendUDP c addr testMsg
                [(_, partial, rcvMsg)]<- recvUDP defaultUDPRecvConfig s
                partial @=? False
                rcvMsg @=? testMsg

                threadDelay 100000

                forkIO $ sendUDP c addr longMsg
                [(_, partial, rcvMsg)]<- recvUDP defaultUDPRecvConfig s
                partial @=? True

    it "UDP sending addr test" $ do
        let testMsg = V.replicate 256 48
            addr = SocketAddrIPv4 ipv4Loopback 12346
            addr' = SocketAddrIPv4 ipv4Loopback 12347
        withResource (initUDP defaultUDPConfig{udpLocalAddr = Just (addr,UDP_DEFAULT)}) $ \ c ->
            withResource (initUDP defaultUDPConfig{udpLocalAddr = Just (addr',UDP_DEFAULT)}) $ \ s -> do
                forkIO $ sendUDP c addr' testMsg
                [(rcvAddr, _, _)]<- recvUDP defaultUDPRecvConfig s
                Just addr @=? rcvAddr

    it "overlong message exception" $ do
        let testMsg = V.replicate 4096 48
            addr = SocketAddrIPv4 ipv4Loopback 12348
        withResource (initUDP defaultUDPConfig) $ \ c ->
            withResource (initUDP defaultUDPConfig) $ \_s -> do
                sendUDP c addr testMsg `shouldThrow` anyException

    it "batch receiving(multiple messages)" $ do
        let testMsg = V.replicate 256 48
            addr = SocketAddrIPv4 ipv4Loopback 12346
        msgList <- newIORef []
        forkIO $ withResource (initUDP defaultUDPConfig{udpLocalAddr = Just (addr,UDP_DEFAULT)}) $ \ s -> do
            recvUDPLoop defaultUDPRecvConfig s $ \ msgs ->
                modifyIORef msgList (msgs:)
        withResource (initUDP defaultUDPConfig) $ \c -> replicateM_ 100 $ sendUDP c addr testMsg
        msgs <- readIORef msgList
        True @=? (List.length msgs > 50)    -- udp packet may get lost
        forM_ msgs $ \ (_,_,msg) -> testMsg @=? msg
