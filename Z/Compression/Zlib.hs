{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImplicitParams #-}

{-|
Module      : Z.Compression.Zlib
Description : The zlib
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides <https://zlib.net zlib> bindings.

-}

module Z.Compression.Zlib
  (
  ,
  ) where

import Z.Foreign

data ZlibException
    = ZDataError
    |

{- | The 'WindowBits' is the base two logarithm of the maximum window size (the size of the history buffer).

It should be in the range 8..15 for this version of the library. The 'defaultWindowBits' value is 15. Decompressing windowBits must be greater than or equal to the compressing windowBits. If a compressed stream with a larger window size is given as input, decompress will throw 'ZDataError'

windowBits can also be –8..–15 for raw inflate. In this case, -windowBits determines the window size. inflate() will then process raw deflate data, not looking for a zlib or gzip header, not generating a check value, and not looking for any check values for comparison at the end of the stream.

windowBits can also be greater than 15 for optional gzip decoding. Add 32 to windowBits to enable zlib and gzip decoding with automatic header detection, or add 16 to decode only the gzip format.
-}
type WindowBits = CInt

defaultWindowBits :: WindowBits
defaultWindowBits = 15

data CompressConfig = CompressConfig
    { compressLevel :: CInt
    , compressWindowBits :: WindowBits
    , compressMemoryLevel :: CInt
    , compressDictionary :: CBytes
    , compressStrategy :: CInt
    }

compress :: CompressConfig
         -> IO V.Bytes
         -> IO V.Bytes
compress

compressPure :: CompressConfig -> V.Bytes -> V.Bytes
compressPure =

data DecompressConfig = DecompressConfig
    { decompressWindowBits :: WindowBits
    , decompressDictionary :: CBytes
    }

decompress :: DecompressConfig
           -> IO V.Bytes
           -> IO V.Bytes
decompress =

decompressPure :: DecompressConfig -> V.Bytes -> V.Bytes



--------------------------------------------------------------------------------
-- | We use a plain GHC heap MutableByteArray# to represent zstream struct
newtype ZStream = MutablePrimArray RealWorld ()

newZStream :: WindowBit -> IO ZStream
newZStream = new
