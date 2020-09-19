{-# LANGUAGE OverloadedStrings #-}

module Z.IO.UDPSpec where

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
import           Z.IO.Network.SocketAddr
import           Z.IO.Network.UDP
import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = describe "UDP operations" $ do
    it "roundtrip test" $ do
        let testMsg = V.replicate 256 48
            longMsg = V.replicate 2048 48
            addr = SocketAddrInet 12345 inetLoopback
        withResource (initUDP defaultUDPConfig{udpSendMsgSize = 2048}) $ \ c ->
            withResource (initUDP defaultUDPConfig{udpLocalAddr = Just (addr,UV_UDP_DEFAULT)}) $ \ s -> do
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
            addr = SocketAddrInet 12346 inetLoopback
            addr' = SocketAddrInet 12347 inetLoopback
        withResource (initUDP defaultUDPConfig{udpLocalAddr = Just (addr,UV_UDP_DEFAULT)}) $ \ c ->
            withResource (initUDP defaultUDPConfig{udpLocalAddr = Just (addr',UV_UDP_DEFAULT)}) $ \ s -> do
                forkIO $ sendUDP c addr' testMsg
                [(rcvAddr, _, _)]<- recvUDP defaultUDPRecvConfig s
                Just addr @=? rcvAddr

    it "overlong message exception" $ do
        let testMsg = V.replicate 4096 48
            addr = SocketAddrInet 12348 inetLoopback
        withResource (initUDP defaultUDPConfig) $ \ c ->
            withResource (initUDP defaultUDPConfig) $ \ s -> do
                sendUDP c addr testMsg `shouldThrow` anyException
