{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Z.IO.Network
import Z.IO.Resource
import Z.IO.Buffered
import Z.IO.Exception
import qualified Z.Data.CBytes as CBytes
import Foreign.ForeignPtr
import GHC.Ptr

main :: IO ()
main = do
    let conf = defaultTCPServerConfig{
            tcpServerWorker = \ uvs ->  do
                recvbuf <- mallocForeignPtrBytes 2048
                -- we reuse buffer as golang does,
                -- since node use slab, which is in fact a memory pool
                -- this is more fair

                -- do not print ECONNRESET for fairness
                catch (echo uvs recvbuf) (\ (e::SomeException) -> return ())
        }

    -- let addr = ipv4 "204.79.197.200" 80
    -- withResource (initTCPClient defaultTCPClientConfig{tcpRemoteAddr = addr}) $ \ tcp -> print tcp

    startTCPServer conf
  where
    echo uvs recvbuf = loop
      where
        loop = do
            r <- withForeignPtr recvbuf $ \ p -> do
                readInput uvs p 2048
            when (r /= 0) $ do
                CBytes.withCBytes sendbuff $ \ sendbuffp -> do
                    writeOutput uvs (castPtr sendbuffp) sendbuffl
                    loop

    sendbuffl = CBytes.length sendbuff
    sendbuff =
        "HTTP/1.1 200 OK\r\n\
        \Content-Type: text/html; charset=UTF-8\r\n\
        \Content-Length: 500\r\n\
        \Connection: Keep-Alive\r\n\
        \\r\n" `CBytes.append` (CBytes.pack $ replicate 500 '0')




