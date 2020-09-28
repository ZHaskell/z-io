{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Z.IO.Network
import Z.IO.Resource
import Z.IO.Buffered
import Z.IO.Exception
import qualified Z.Data.Vector as V
import qualified Z.Foreign as FFI
import Foreign.ForeignPtr
import GHC.Ptr

main :: IO ()
main = do
    let conf = defaultTCPServerConfig{
            tcpServerWorker = \ uvs ->  do
                recvbuf <- mallocForeignPtrBytes 2048
                sendcontent' <- FFI.pinPrimVector sendcontent
                catch (echo uvs recvbuf sendcontent') (\ (e::SomeException) -> return ())
        }

    -- let addr = ipv4 "204.79.197.200" 80
    -- withResource (initTCPClient defaultTCPClientConfig{tcpRemoteAddr = addr}) $ \ tcp -> print tcp

    startTCPServer conf
  where
    echo uvs recvbuf sendcontent' = loop
      where
        loop = do
            r <- withForeignPtr recvbuf $ \ p -> do
                readInput uvs p 2048
            when (r /= 0) $ do
                FFI.withPrimVectorSafe. sendcontent' $ \ sendbuffp -> do
                    writeOutput uvs (castPtr sendbuffp) sendbuffl
                    loop

    sendcontent = V.Bytes
    sendcontent =
        "HTTP/1.1 200 OK\r\n\
        \Content-Type: text/html; charset=UTF-8\r\n\
        \Content-Length: 500\r\n\
        \Connection: Keep-Alive\r\n\
        \\r\n" `V.append` (V.pack $ replicate 500 48)




