{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Z.Compression.Zlib where

import Data.Typeable
import Control.Monad
import Z.Data.Array as A
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import GHC.Stack
import Z.Data.CBytes as CBytes
import Z.Data.Vector.Base as V
import Z.IO.Exception
import Z.Foreign
import Z.IO.Buffered (drain)
import System.IO.Unsafe (unsafePerformIO)

#include "hs_zlib.h" 

newtype Strategy = Strategy CInt deriving (Eq, Ord, Show, Typeable)

pattern Z_FILTERED           = Strategy (#const Z_FILTERED        ) 
pattern Z_HUFFMAN_ONLY       = Strategy (#const Z_HUFFMAN_ONLY    )  
pattern Z_RLE                = Strategy (#const Z_RLE             )  
pattern Z_FIXED              = Strategy (#const Z_FIXED           )  
pattern Z_DEFAULT_STRATEGY   = Strategy (#const Z_DEFAULT_STRATEGY)  


newtype CompressLevel = CompressLevel CInt deriving (Eq, Ord, Show, Typeable)

-- pattern Z_NO_COMPRESSION       =  CompressLevel (#const Z_NO_COMPRESSION     )
pattern Z_BEST_SPEED           =  CompressLevel (#const Z_BEST_SPEED         )
pattern Z_BEST_COMPRESSION     =  CompressLevel (#const Z_BEST_COMPRESSION   )
pattern Z_DEFAULT_COMPRESSION  =  CompressLevel (#const Z_DEFAULT_COMPRESSION)

{- | The 'WindowBits' is the base two logarithm of the maximum window size (the size of the history buffer).

It should be in the range 8..15 for this version of the library. The 'defaultWindowBits' value is 15. Decompressing windowBits must be greater than or equal to the compressing windowBits. If a compressed stream with a larger window size is given as input, decompress will throw 'ZDataError'

windowBits can also be –8..–15 for raw inflate. In this case, -windowBits determines the window size. inflate() will then process raw deflate data, not looking for a zlib or gzip header, not generating a check value, and not looking for any check values for comparison at the end of the stream.

windowBits can also be greater than 15 for optional gzip decoding. Add 32 to windowBits to enable zlib and gzip decoding with automatic header detection, or add 16 to decode only the gzip format.
-}
newtype WindowBits = WindowBits CInt
    deriving (Eq, Ord, Read, Show, Num, Typeable)

defaultWindowBits :: WindowBits
defaultWindowBits = WindowBits 15

-- | The 'MemLevel' specifies how much memory should be allocated for the internal compression state. 1 uses minimum memory but is slow and reduces compression ratio; 9 uses maximum memory for optimal speed. The default value is 8.
newtype MemLevel = MemLevel CInt
    deriving (Eq, Ord, Read, Show, Num, Typeable)

defaultMemLevel :: MemLevel
defaultMemLevel = MemLevel 9

data CompressConfig = CompressConfig
    { compressLevel :: CompressLevel
    , compressWindowBits :: WindowBits
    , compressMemoryLevel :: MemLevel
    , compressDictionary :: CBytes
    , compressStrategy :: Strategy
    }

defaultCompressConfig :: CompressConfig
defaultCompressConfig = 
    CompressConfig Z_DEFAULT_COMPRESSION  defaultWindowBits
        defaultMemLevel CBytes.empty Z_DEFAULT_STRATEGY

compressIO :: HasCallStack 
           => CompressConfig
           -> IO V.Bytes        -- ^ input, 'V.empty' indicate EOF
           -> IO (IO V.Bytes)   -- ^ output, 'V.empty' indicate EOF
compressIO (CompressConfig level windowBits memLevel dict strategy) src = do
    allocMutableByteArrayUnsafe (#size z_stream) $ \ ps -> do
        -- use safe here because it have to persist between deflates
        throwZlibIfMinus_ $
            deflateInit2 ps level (#const Z_DEFLATED) windowBits memLevel strategy
        unless (CBytes.null dict) (setDict ps)
        (A.MutablePrimArray buf :: A.MutablePrimArray RealWorld Word8) <- A.newPinnedPrimArray bSiz
        set_avail_out ps buf (fromIntegral bSiz)
        return (pop ps buf)
  where
    bSiz = V.defaultChunkSize
    setDict ps = throwZlibIfMinus_ . withCBytes dict $ \ pdict ->
        deflateSetDictionary ps pdict (fromIntegral $ CBytes.length dict)
    pop ps buf = do
        oavail <- get_avail_out ps
        if oavail == 0
        then do
            oarr <- A.unsafeFreezeArr (A.MutablePrimArray buf) 
            (A.MutablePrimArray buf' :: A.MutablePrimArray RealWorld Word8) <- A.newPinnedPrimArray bSiz
            set_avail_out ps buf' (fromIntegral bSiz)
            draw ps buf'
            return (V.PrimVector oarr 0 bSiz)
        else draw ps buf
    draw ps buf = do
        iavail <- get_avail_in ps
        if iavail == 0
        then do
            input <- src 
            if V.null input
            then V.concat <$> finish ps buf []
            else do
                -- use safe here because it may have to persist between deflates
                withPrimVectorSafe input $ \ pi len -> do
                    set_avail_in ps pi (fromIntegral len)
                    throwZlibIfMinus_ (deflate ps (#const Z_NO_FLUSH))
                    pop ps buf
        else do
            throwZlibIfMinus_ (deflate ps (#const Z_NO_FLUSH))
            pop ps buf

    finish ps buf acc = do
        r <- throwZlibIfMinus (deflate ps (#const Z_FINISH))
        oavail <- get_avail_out ps
        oarr <- A.unsafeFreezeArr (A.MutablePrimArray buf)
        let v = (V.PrimVector oarr 0 (bSiz - fromIntegral oavail))
        if r == (#const Z_STREAM_END)
        then return $ reverse (v:acc)
        else do
            (A.MutablePrimArray buf' :: A.MutablePrimArray RealWorld Word8) <- A.newPinnedPrimArray bSiz
            set_avail_out ps buf' (fromIntegral bSiz)
            finish ps buf' (v:acc)




    
compress :: CompressConfig -> V.Bytes -> V.Bytes
compress conf i = unsafePerformIO $ do
    popper <- compressIO conf (return i)
    V.concat <$> drain V.null popper
        

data DecompressConfig = DecompressConfig
    { decompressWindowBits :: WindowBits
    , decompressDictionary :: CBytes
    }

{-
decompressIO :: DecompressConfig
             -> IO V.Bytes
             -> IO V.Bytes
decompressIO =

decompress :: DecompressConfig -> V.Bytes -> V.Bytes
-}


--------------------------------------------------------------------------------

newtype ZReturn = ZReturn CInt deriving (Eq, Ord, Show, Typeable)

toZErrorMsg :: CInt -> CBytes
toZErrorMsg (#const Z_OK           ) =  "Z_OK"
toZErrorMsg (#const Z_STREAM_END   ) =  "Z_STREAM_END"
toZErrorMsg (#const Z_NEED_DICT    ) =  "Z_NEED_DICT"
toZErrorMsg (#const Z_ERRNO        ) =  "Z_ERRNO"
toZErrorMsg (#const Z_STREAM_ERROR ) =  "Z_STREAM_ERROR"
toZErrorMsg (#const Z_DATA_ERROR   ) =  "Z_DATA_ERROR"
toZErrorMsg (#const Z_MEM_ERROR    ) =  "Z_MEM_ERROR"
toZErrorMsg (#const Z_BUF_ERROR    ) =  "Z_BUF_ERROR"
toZErrorMsg (#const Z_VERSION_ERROR) =  "Z_VERSION_ERROR"
toZErrorMsg _                        =  "Z_UNEXPECTED"

data ZlibException = ZlibException CBytes CallStack deriving (Show, Typeable)
instance Exception ZlibException

throwZlibIfMinus :: HasCallStack => IO CInt -> IO CInt
throwZlibIfMinus f = do
    r <- f
    if r < 0 then throwIO (ZlibException (toZErrorMsg r) callStack)
             else return r

throwZlibIfMinus_ :: HasCallStack => IO CInt -> IO ()
throwZlibIfMinus_ = void . throwZlibIfMinus

data ZStream

-- deflateInit2 (z_streamp strm, int  level, int method, int windowBits, int memLevel, int strategy)
foreign import ccall unsafe
    deflateInit2 :: MBA## ZStream -> CompressLevel -> CInt -> WindowBits -> MemLevel -> Strategy -> IO CInt

foreign import ccall unsafe
    deflateSetDictionary :: MBA## ZStream -> CString -> CUInt -> IO CInt

foreign import ccall unsafe
    deflate :: MBA## ZStream -> CInt -> IO CInt

foreign import ccall unsafe
    set_avail_in :: MBA## ZStream -> Ptr Word8 -> CUInt -> IO ()

foreign import ccall unsafe
    set_avail_out :: MBA## ZStream -> MBA## Word8 -> CUInt -> IO ()

foreign import ccall unsafe
    get_avail_out :: MBA## ZStream -> IO CUInt

foreign import ccall unsafe
    get_avail_in :: MBA## ZStream -> IO CUInt
