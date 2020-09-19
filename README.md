<p align=center>
  <img src="https://github.com/haskell-Z/Z/raw/master/projectZ.svg">
</p>

## Z-IO

[![Linux Build Status](https://img.shields.io/travis/haskell-z/z-io/master.svg?label=Linux%20build)](https://travis-ci.org/haskell-z/z-io)

This package provides basic IO operations:

* IO resource management, resource pool
* File system
* Network: DNS, TCP, UDP and IPC
* Buffered input and output
* High performance logger
* High performance timer

## Dependencies

On *nix system, libuv >= 1.32 are required to build this library, e.g.

```base
# on ubuntu
sudo apt-get install libuv1 libuv1-dev
# on mac
brew install libuv
```


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
| in withResource (initTCPClient defaultTCPClientConfig{ tcpRemoteAddr = addr}) $ \ tcp -> do
|     i <- newBufferedInput defaultChunkSize tcp 
|     o <- newBufferedOutput defaultChunkSize tcp
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
|     i <- newBufferedInput defaultChunkSize tcp 
|     o <- newBufferedOutput defaultChunkSize tcp
|     forever $ readBuffer i >>= writeBuffer o >> flushBuffer o
| }
| :}
```

## Dev guide

+ GHC(>=8.10.2) 
+ cabal-install(>=3.4)

```bash
# get code
git clone --recursive git@github.com:haskell-Z/z-io.git 
cd z-data
# build
cabal build
# test
cabal run Z-IO-Test
# install 
cabal install
# generate document
cabal haddock
```
