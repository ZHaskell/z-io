<p align=center>
  <img src="https://github.com/haskell-Z/Z/raw/master/projectZ.svg">
</p>

## Z-IO

[![Linux Build Status](https://github.com/haskell-Z/z-io/workflows/ubuntu-ci/badge.svg)](https://github.com/haskell-Z/z-io/actions) [![MacOS Build Status](https://github.com/haskell-Z/z-io/workflows/oxs-ci/badge.svg)](https://github.com/haskell-Z/z-io/actions) [![Windows Build Status](https://github.com/haskell-Z/z-io/workflows/win-ci/badge.svg)](https://github.com/haskell-Z/z-io/actions)

This package provides basic IO operations:

* IO resource management, resource pool
* File system
* Network: DNS, TCP, UDP and IPC
* Buffered input and output
* High performance logger
* High performance timer

## Requirements

* A working haskell compiler system, GHC(>=8.6), cabal-install(>=2.4), hsc2hs.

* Tests need  [hspec-discover](https://hackage.haskell.org/package/hspec-discover).

## Example usage

```haskell
> :set -XOverloadedStrings  
> import Z.IO.Network
> import Z.IO.Resource
> import Z.IO.Buffered
> 
> -- call getAddrInfo to perform DNS
> head <$> getAddrInfo Nothing "www.bing.com" "http"
AddrInfo {addrFlags = [AI_ADDRCONFIG,AI_V4MAPPED], addrFamily = SocketFamily 2, addrSocketType = SocketType 1, addrProtocol = ProtocolNumber 6, addrAddress = 204.79.197.200:80, addrCanonName = }
>
> import qualified Z.Data.Text as T
> -- send a simple HTTP request
> :{
| let addr = SocketAddrInet 80 (tupleToInetAddr (13,107,21,200))
| -- addr = ipv4 "13.107.21.200" 80
| in withResource (initTCPClient defaultTCPClientConfig{ tcpRemoteAddr = addr}) $ \ tcp -> do
|     i <- newBufferedInput tcp 
|     o <- newBufferedOutput tcp
|     writeBuffer o "GET http://www.bing.com HTTP/1.1\r\nHost: www.bing.com\r\n\r\n"
|     flushBuffer o
|     readBuffer i >>= pure . T.validate
| :}
"HTTP/1.1 200 OK\r\nDate: Sat, 19 Sep 2020 06:11:08 GMT\r\nContent-Length: 0\r\n\r\n"
>
> -- Start a TCP echo server, use @nc -v localhost 8080@ to test
> :{
| startTCPServer defaultTCPServerConfig{
| tcpListenAddr = SocketAddrInet 8080 inetLoopback,
| tcpServerWorker = \ tcp -> do
|     i <- newBufferedInput tcp 
|     o <- newBufferedOutput tcp
|     forever $ readBuffer i >>= writeBuffer o >> flushBuffer o
| }
| :}
```

## Dev guide


```bash
# get code
git clone --recursive git@github.com:haskell-Z/z-io.git 
cd z-io
# build
cabal build
# test
cabal run Z-IO-Test
# install 
cabal install
# generate document
cabal haddock
```
