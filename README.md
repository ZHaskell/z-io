## Z-IO

[![Hackage](https://img.shields.io/hackage/v/Z-IO.svg?style=flat)](https://hackage.haskell.org/package/Z-IO)
[![Linux Build Status](https://github.com/haskell-Z/z-io/workflows/ubuntu-ci/badge.svg)](https://github.com/haskell-Z/z-io/actions)
[![MacOS Build Status](https://github.com/haskell-Z/z-io/workflows/osx-ci/badge.svg)](https://github.com/haskell-Z/z-io/actions)
[![Windows Build Status](https://github.com/haskell-Z/z-io/workflows/win-ci/badge.svg)](https://github.com/haskell-Z/z-io/actions)
[![Docker Build Status](https://github.com/haskell-Z/z-io/workflows/docker-ci/badge.svg)](https://github.com/haskell-Z/z-io/actions)

This package is part of [Z](https://github.com/haskell-Z/Z) project, provides basic IO operations:

* IO resource management, resource pool
* File system operations
* Network: DNS, TCP, UDP and IPC
* Buffered input and output
* Process management
* Environment settings
* High performance logger
* High performance low resolution timer

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
let addr = ipv4 "13.107.21.200" 80
in withResource (initTCPClient defaultTCPClientConfig{ tcpRemoteAddr = addr}) $ \ tcp -> do
    i <- newBufferedInput tcp
    o <- newBufferedOutput tcp
    writeBuffer o "GET http://www.bing.com HTTP/1.1\r\nHost: www.bing.com\r\n\r\n"
    flushBuffer o
    readBuffer i >>= pure . T.validate
:}
"HTTP/1.1 200 OK\r\nDate: Sat, 19 Sep 2020 06:11:08 GMT\r\nContent-Length: 0\r\n\r\n"
>
> -- Start a TCP echo server, use @nc -v localhost 8080@ to test
> :{
startTCPServer defaultTCPServerConfig{
    tcpListenAddr = SocketAddrIPv4 ipv4Loopback 8080} $ \ tcp -> do
        i <- newBufferedInput tcp
        o <- newBufferedOutput tcp
        forever $ readBuffer i >>= writeBuffer o >> flushBuffer o
}
:}
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
