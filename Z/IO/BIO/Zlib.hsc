{-|
Module      : Z.IO.BIO.Zlib
Description : The zlib
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides <https://zlib.net zlib> bindings using 'BIO' interface.
-}

module Z.IO.BIO.Zlib(
  -- * Compression
    CompressConfig(..)
  , defaultCompressConfig
  , newCompress, newCompress', compressReset
  , compress
  , compressBlocks
  , WindowBits
  , defaultWindowBits
  , MemLevel
  , defaultMemLevel
  -- * Decompression
  , DecompressConfig(..)
  , defaultDecompressConfig
  , newDecompress, newDecompress', decompressReset
  , decompress
  , decompressBlocks
  -- * Constants
  -- ** Strategy
  , Strategy
  , pattern Z_FILTERED
  , pattern Z_HUFFMAN_ONLY
  , pattern Z_RLE
  , pattern Z_FIXED
  , pattern Z_DEFAULT_STRATEGY
  -- ** CompressLevel
  , CompressLevel
  , pattern Z_BEST_SPEED
  , pattern Z_BEST_COMPRESSION
  , pattern Z_DEFAULT_COMPRESSION
  ) where

import           Control.Monad
import           Data.IORef
import           Data.Typeable
import           Data.Word
import qualified Data.List          as List
import           Foreign            hiding (void)
import           Foreign.C
import           GHC.Generics
import           Z.Data.Array       as A
import           Z.Data.CBytes      as CBytes
import           Z.Data.Vector.Base as V
import           Z.Data.Text.ShowT  (ShowT)
import           Z.Foreign
import           Z.IO.BIO
import           Z.IO.Exception

#include "zlib.h"

type Strategy = CInt

pattern Z_FILTERED           :: Strategy
pattern Z_HUFFMAN_ONLY       :: Strategy
pattern Z_RLE                :: Strategy
pattern Z_FIXED              :: Strategy
pattern Z_DEFAULT_STRATEGY   :: Strategy
pattern Z_FILTERED           = #const Z_FILTERED
pattern Z_HUFFMAN_ONLY       = #const Z_HUFFMAN_ONLY
pattern Z_RLE                = #const Z_RLE
pattern Z_FIXED              = #const Z_FIXED
pattern Z_DEFAULT_STRATEGY   = #const Z_DEFAULT_STRATEGY

type CompressLevel = CInt

-- pattern Z_NO_COMPRESSION       =  CompressLevel (#const Z_NO_COMPRESSION     )
pattern Z_BEST_SPEED          :: CompressLevel
pattern Z_BEST_COMPRESSION    :: CompressLevel
pattern Z_DEFAULT_COMPRESSION :: CompressLevel
pattern Z_BEST_SPEED          = #const Z_BEST_SPEED
pattern Z_BEST_COMPRESSION    = #const Z_BEST_COMPRESSION
pattern Z_DEFAULT_COMPRESSION = #const Z_DEFAULT_COMPRESSION

{- | The 'WindowBits' is the base two logarithm of the maximum window size (the size of the history buffer).
It should be in the range 8..15 for this version of the library. The 'defaultWindowBits' value is 15. Decompressing windowBits must be greater than or equal to the compressing windowBits. If a compressed stream with a larger window size is given as input, decompress will throw 'ZDataError'
windowBits can also be –8..–15 for raw inflate. In this case, -windowBits determines the window size. inflate() will then process raw deflate data, not looking for a zlib or gzip header, not generating a check value, and not looking for any check values for comparison at the end of the stream.
windowBits can also be greater than 15 for optional gzip decoding. Add 32 to windowBits to enable zlib and gzip decoding with automatic header detection, or add 16 to decode only the gzip format.
-}
type WindowBits = CInt

defaultWindowBits :: WindowBits
defaultWindowBits = 15

-- | The 'MemLevel' specifies how much memory should be allocated for the internal compression state. 1 uses minimum memory but is slow and reduces compression ratio; 9 uses maximum memory for optimal speed. The default value is 8.
type MemLevel = CInt

defaultMemLevel :: MemLevel
defaultMemLevel = 9

data CompressConfig = CompressConfig
    { compressLevel :: CompressLevel
    , compressWindowBits :: WindowBits
    , compressMemoryLevel :: MemLevel
    , compressDictionary :: V.Bytes
    , compressStrategy :: Strategy
    , compressBufferSize :: Int
    }   deriving (Show, Eq, Ord, Generic)
        deriving anyclass ShowT

defaultCompressConfig :: CompressConfig
defaultCompressConfig =
    CompressConfig Z_DEFAULT_COMPRESSION  defaultWindowBits
        defaultMemLevel V.empty Z_DEFAULT_STRATEGY V.defaultChunkSize

data ZStream = ZStream (ForeignPtr ZStream) (IORef Bool)

-- | Compress all the data written to a output.
--
-- The returned 'BIO' node can not be reused after get flushed.
newCompress :: HasCallStack
            => CompressConfig
            -> IO (BIO V.Bytes V.Bytes)
newCompress conf = snd <$> newCompress' conf

-- | Compress all the data written to a output.
--
-- The returned 'BIO' node can be reused after you call 'compressReset' on the 'ZStream'.
newCompress' :: HasCallStack
             => CompressConfig
             -> IO (ZStream, BIO V.Bytes V.Bytes)
newCompress' (CompressConfig level windowBits memLevel dict strategy bufSiz) = do
    zs <- newForeignPtr free_z_stream_deflate =<< create_z_stream
    buf <- A.newPinnedPrimArray bufSiz
    set_avail_out zs buf bufSiz
    bufRef <- newIORef buf

    withForeignPtr zs $ \ ps -> do
        throwZlibIfMinus_ $ deflate_init2 ps level windowBits memLevel strategy
        unless (V.null dict) $
            throwZlibIfMinus_ . withPrimVectorUnsafe dict $ \ pdict off len ->
            deflate_set_dictionary ps pdict off (fromIntegral $ len)

    finRef <- newIORef False
    return (ZStream zs finRef, BIO (zwrite zs bufRef) (zflush finRef zs bufRef []))
  where
    zwrite zs bufRef input = do
        set_avail_in zs input (V.length input)
        zloop zs bufRef []

    zloop zs bufRef acc = do
        oavail :: CUInt <- withForeignPtr zs $ \ ps -> do
            throwZlibIfMinus_ (deflate ps (#const Z_NO_FLUSH))
            (#peek struct z_stream_s, avail_out) ps
        if oavail == 0
        then do
            oarr <- A.unsafeFreezeArr =<< readIORef bufRef
            buf' <- A.newPinnedPrimArray bufSiz
            set_avail_out zs buf' bufSiz
            writeIORef bufRef buf'
            zloop zs bufRef (V.PrimVector oarr 0 bufSiz : acc)
        else do
            let output = V.concat (List.reverse acc)
            if V.null output then return Nothing
                             else return (Just output)

    zflush finRef zs bufRef acc = do
        fin <- readIORef finRef
        if fin 
        then return Nothing
        else do
            buf <- readIORef bufRef
            (r, osiz) <- withForeignPtr zs $ \ ps -> do
                r <- throwZlibIfMinus (deflate ps (#const Z_FINISH))
                oavail :: CUInt <- (#peek struct z_stream_s, avail_out) ps
                return (r, bufSiz - fromIntegral oavail)
            if (r /= (#const Z_STREAM_END) && osiz /= 0)
            then do
                oarr <- A.unsafeFreezeArr buf
                buf' <- A.newPinnedPrimArray bufSiz
                set_avail_out zs buf' bufSiz
                writeIORef bufRef buf'
                zflush finRef zs bufRef (V.PrimVector oarr 0 osiz : acc)
            else do
                oarr <- A.unsafeFreezeArr buf
                let trailing = V.concat . List.reverse $ V.PrimVector oarr 0 osiz : acc
                -- stream ends 
                writeIORef finRef True
                if V.null trailing then return Nothing else return (Just trailing)

-- | Reset compressor's state so that it can be reused.
compressReset :: ZStream -> IO ()
compressReset (ZStream fp finRef) = do
    throwZlibIfMinus_ (withForeignPtr fp deflateReset)
    writeIORef finRef False

-- | Decompress some bytes.
compress :: HasCallStack => CompressConfig -> V.Bytes -> V.Bytes
compress conf = V.concat . unsafeRunBlock (newCompress conf)

-- | Decompress some bytes list.
compressBlocks :: HasCallStack => CompressConfig -> [V.Bytes] -> [V.Bytes]
compressBlocks conf = unsafeRunBlocks (newCompress conf)

data DecompressConfig = DecompressConfig
    { decompressWindowBits :: WindowBits
    , decompressDictionary :: V.Bytes
    , decompressBufferSize :: Int
    }   deriving (Show, Eq, Ord, Generic)
        deriving anyclass ShowT

defaultDecompressConfig :: DecompressConfig
defaultDecompressConfig = DecompressConfig defaultWindowBits V.empty V.defaultChunkSize

-- | Decompress bytes from source.
--
-- The returned 'BIO' node can not be reused after get flushed.
newDecompress :: DecompressConfig -> IO (BIO V.Bytes V.Bytes)
newDecompress conf = snd <$> newDecompress' conf

-- | Decompress bytes from source.
--
-- The returned 'BIO' node can be reused after you call 'decompressReset' on the 'ZStream'.
newDecompress' :: DecompressConfig -> IO (ZStream, BIO V.Bytes V.Bytes)
newDecompress' (DecompressConfig windowBits dict bufSiz) = do
    zs <- newForeignPtr free_z_stream_inflate =<< create_z_stream
    buf <- A.newPinnedPrimArray bufSiz
    set_avail_out zs buf bufSiz
    bufRef <- newIORef buf
    withForeignPtr zs $ \ ps -> do
        throwZlibIfMinus_ $ inflate_init2 ps windowBits
    finRef <- newIORef False
    return (ZStream zs finRef, BIO (zwrite zs bufRef) (zflush finRef zs bufRef []))
  where
    zwrite zs bufRef input = do
        set_avail_in zs input (V.length input)
        zloop zs bufRef []

    zloop zs bufRef acc = do
        oavail :: CUInt <- withForeignPtr zs $ \ ps -> do
            r <- throwZlibIfMinus (inflate ps (#const Z_NO_FLUSH))
            when (r == (#const Z_NEED_DICT)) $
                if V.null dict
                then throwIO (ZlibException "Z_NEED_DICT" callStack)
                else do
                    throwZlibIfMinus_ . withPrimVectorUnsafe dict $ \ pdict off len ->
                        inflate_set_dictionary ps pdict off (fromIntegral len)
                    throwZlibIfMinus_ (inflate ps (#const Z_NO_FLUSH))
            (#peek struct z_stream_s, avail_out) ps
        if oavail == 0
        then do
            oarr <- A.unsafeFreezeArr =<< readIORef bufRef
            buf' <- A.newPinnedPrimArray bufSiz
            set_avail_out zs buf' bufSiz
            writeIORef bufRef buf'
            zloop zs bufRef (V.PrimVector oarr 0 bufSiz : acc)
        else do
            let output = V.concat (List.reverse acc)
            if V.null output then return Nothing
                             else return (Just output)

    zflush finRef zs bufRef acc = do
        fin <- readIORef finRef
        if fin
        then return Nothing
        else do
            buf <- readIORef bufRef
            (r, osiz) <- withForeignPtr zs $ \ ps -> do
                r <- throwZlibIfMinus (inflate ps (#const Z_FINISH))
                r' <- if r == (#const Z_NEED_DICT)
                then if V.null dict
                    then throwIO (ZlibException "Z_NEED_DICT" callStack)
                    else do
                        throwZlibIfMinus_ . withPrimVectorUnsafe dict $ \ pdict off len ->
                            inflate_set_dictionary ps pdict off (fromIntegral len)
                        throwZlibIfMinus (inflate ps (#const Z_FINISH))
                else return r
                oavail :: CUInt <- (#peek struct z_stream_s, avail_out) ps
                return (r', bufSiz - fromIntegral oavail)
            if (r /= (#const Z_STREAM_END) && osiz /= 0)
            then do
                oarr <- A.unsafeFreezeArr buf
                buf' <- A.newPinnedPrimArray bufSiz
                set_avail_out zs buf' bufSiz
                writeIORef bufRef buf'
                zflush finRef zs bufRef (V.PrimVector oarr 0 osiz : acc)
            else do
                oarr <- A.unsafeFreezeArr buf
                let trailing = V.concat . List.reverse $ V.PrimVector oarr 0 osiz : acc
                -- stream ends
                writeIORef finRef True
                if V.null trailing then return Nothing else return (Just trailing)

-- | Reset decompressor's state so that it can be reused.
decompressReset :: ZStream -> IO ()
decompressReset (ZStream fp finRef) = do
    throwZlibIfMinus_ (withForeignPtr fp inflateReset)
    writeIORef finRef False

-- | Decompress some bytes.
decompress :: HasCallStack => DecompressConfig -> V.Bytes -> V.Bytes
decompress conf = V.concat . unsafeRunBlock (newDecompress conf)

-- | Decompress some bytes list.
decompressBlocks :: HasCallStack => DecompressConfig -> [V.Bytes] -> [V.Bytes]
decompressBlocks conf = unsafeRunBlocks (newDecompress conf)

--------------------------------------------------------------------------------

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

-- | Zlib exceptions, a sub exception type to 'SomeIOException'.
data ZlibException = ZlibException CBytes CallStack deriving (Show, Typeable)
instance Exception ZlibException where
    toException = ioExceptionToException
    fromException = ioExceptionFromException

throwZlibIfMinus :: HasCallStack => IO CInt -> IO CInt
throwZlibIfMinus f = do
    r <- f
    if r < 0 && r /= (#const Z_BUF_ERROR)
    then throwIO (ZlibException (toZErrorMsg r) callStack)
    else return r

throwZlibIfMinus_ :: HasCallStack => IO CInt -> IO ()
throwZlibIfMinus_ = void . throwZlibIfMinus

foreign import ccall unsafe
    create_z_stream :: IO (Ptr ZStream)

foreign import ccall unsafe "hs_zlib.c &free_z_stream_inflate"
    free_z_stream_inflate :: FunPtr (Ptr ZStream -> IO ())

foreign import ccall unsafe "hs_zlib.c &free_z_stream_deflate"
    free_z_stream_deflate :: FunPtr (Ptr ZStream -> IO ())

foreign import ccall unsafe
    deflate_init2 :: Ptr ZStream -> CompressLevel -> WindowBits -> MemLevel -> Strategy -> IO CInt

foreign import ccall unsafe
    deflate_set_dictionary :: Ptr ZStream -> BA## Word8 -> Int -> Int -> IO CInt

foreign import ccall unsafe
    deflate :: Ptr ZStream -> CInt -> IO CInt

foreign import ccall unsafe
    deflateReset :: Ptr ZStream -> IO CInt

foreign import ccall unsafe
    inflate_init2 :: Ptr ZStream -> WindowBits -> IO CInt

foreign import ccall unsafe
    inflate_set_dictionary :: Ptr ZStream -> BA## Word8 -> Int -> Int -> IO CInt

foreign import ccall unsafe
    inflate :: Ptr ZStream -> CInt -> IO CInt

foreign import ccall unsafe
    inflateReset :: Ptr ZStream -> IO CInt

set_avail_in :: ForeignPtr ZStream -> V.Bytes -> Int -> IO ()
set_avail_in zs buf buflen = do
    withPrimVectorSafe buf $ \ pbuf _ ->
        withForeignPtr zs $ \ ps -> do
            (#poke struct z_stream_s, next_in) ps pbuf
            (#poke struct z_stream_s, avail_in) ps (fromIntegral buflen :: CUInt)

set_avail_out :: ForeignPtr ZStream -> MutablePrimArray RealWorld Word8 -> Int -> IO ()
set_avail_out zs buf bufSiz = do
    withMutablePrimArrayContents buf $ \ pbuf ->
        withForeignPtr zs $ \ ps -> do
            (#poke struct z_stream_s, next_out) ps pbuf
            (#poke struct z_stream_s, avail_out) ps (fromIntegral bufSiz :: CUInt)
