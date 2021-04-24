{-|
Module      : Z.IO.BIO.Zlib
Description : The zlib binding
Copyright   : (c) Dong Han, 2017-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides <https://zlib.net zlib> bindings, with 'BIO' streaming interface, e.g.

@
-- add compressor to your BIO chain to compress streaming blocks of 'V.Bytes'.
(_, zlibCompressor) <- newCompress defaultCompressConfig{compressWindowBits = 31}
runBIO $ src . zlibCompressor . sink
@

-}

module Z.IO.BIO.Zlib(
  -- * Compression
    newCompress, compressReset
  , compress
  , compressBlocks
  , ZStream
  , CompressConfig(..)
  , defaultCompressConfig
  -- * Decompression
  , newDecompress, decompressReset
  , decompress
  , decompressBlocks
  , DecompressConfig(..)
  , defaultDecompressConfig
  -- * Constants
  -- ** Windows bits
  , WindowBits
  , defaultWindowBits
  -- ** Memory level
  , MemLevel
  , defaultMemLevel
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
import           Data.Word
import           Foreign            hiding (void)
import           Foreign.C
import           GHC.Generics
import           Z.Data.Array       as A
import           Z.Data.CBytes      as CBytes
import           Z.Data.JSON        (JSON)
import           Z.Data.Text.Print  (Print)
import           Z.Data.Vector.Base as V
import           Z.Foreign
import           Z.Foreign.CPtr
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
        deriving anyclass (Print, JSON)

defaultCompressConfig :: CompressConfig
defaultCompressConfig =
    CompressConfig Z_DEFAULT_COMPRESSION  defaultWindowBits
        defaultMemLevel V.empty Z_DEFAULT_STRATEGY V.defaultChunkSize

-- | A foreign pointer to a zlib\'s @z_stream_s@ struct.
newtype ZStream = ZStream (CPtr ZStream) deriving (Eq, Ord, Show)
                                         deriving newtype Print

-- | Make a new compress node.
--
-- The returned 'BIO' node can be reused only if you call 'compressReset' on the 'ZStream'.
newCompress :: HasCallStack
            => CompressConfig
            -> IO (ZStream, BIO V.Bytes V.Bytes)
newCompress (CompressConfig level windowBits memLevel dict strategy bufSiz) = do
    zs <- newCPtr'
        (do ps <- throwOOMIfNull create_z_stream
            throwZlibIfMinus_ $ deflate_init2 ps level windowBits memLevel strategy
            return ps)
        free_z_stream_deflate

    unless (V.null dict) .  withCPtr zs $ \ ps -> do
        throwZlibIfMinus_ . withPrimVectorUnsafe dict $ \ pdict off len ->
            deflate_set_dictionary ps pdict off (fromIntegral $ len)

    buf <- A.newPinnedPrimArray bufSiz
    bufRef <- newIORef buf
    set_avail_out zs buf bufSiz

    let newOutBuffer = do
            buf' <- A.newPinnedPrimArray bufSiz
            writeIORef bufRef buf'
            set_avail_out zs buf' bufSiz

    return (ZStream zs, \ k mbs -> case mbs of
        Just bs -> do
            set_avail_in zs bs (V.length bs)
            let loop = do
                    oavail :: CUInt <- withCPtr zs $ \ ps -> do
                        throwZlibIfMinus_ (deflate ps (#const Z_NO_FLUSH))
                        (#peek struct z_stream_s, avail_out) ps
                    when (oavail == 0) $ do
                        oarr <- A.unsafeFreezeArr =<< readIORef bufRef
                        k (Just (V.PrimVector oarr 0 bufSiz))
                        newOutBuffer           
                        loop
            loop
        _ -> 
            let loop = do
                    (r, osiz) <- withCPtr zs $ \ ps -> do
                        r <- throwZlibIfMinus (deflate ps (#const Z_FINISH))
                        oavail :: CUInt <- (#peek struct z_stream_s, avail_out) ps
                        return (r, bufSiz - fromIntegral oavail)
                    if (r /= (#const Z_STREAM_END) && osiz /= 0)
                    then do
                        oarr <- A.unsafeFreezeArr =<< readIORef bufRef
                        k (Just (V.PrimVector oarr 0 osiz))
                        newOutBuffer
                        loop
                    else do
                        -- stream ends
                        when (osiz /= 0) $ do
                            oarr <- A.unsafeFreezeArr =<< readIORef bufRef
                            k (Just (V.PrimVector oarr 0 osiz))
                        k EOF
            in loop)

-- | Reset compressor's state so that related 'BIO' can be reused.
compressReset :: ZStream -> IO ()
compressReset (ZStream fp) = do
    throwZlibIfMinus_ (withCPtr fp deflateReset)

-- | Compress some bytes.
compress :: HasCallStack => CompressConfig -> V.Bytes -> V.Bytes
compress conf = V.concat . unsafeRunBlock (snd <$> newCompress conf)

-- | Compress some bytes in blocks.
compressBlocks :: HasCallStack => CompressConfig -> [V.Bytes] -> [V.Bytes]
compressBlocks conf = unsafeRunBlocks (snd <$> newCompress conf)

data DecompressConfig = DecompressConfig
    { decompressWindowBits :: WindowBits
    , decompressDictionary :: V.Bytes
    , decompressBufferSize :: Int
    }   deriving (Show, Eq, Ord, Generic)
        deriving anyclass (Print, JSON)

defaultDecompressConfig :: DecompressConfig
defaultDecompressConfig = DecompressConfig defaultWindowBits V.empty V.defaultChunkSize

-- | Make a new decompress node.
--
-- The returned 'BIO' node can be reused only if you call 'decompressReset' on the 'ZStream'.
newDecompress :: DecompressConfig -> IO (ZStream, BIO V.Bytes V.Bytes)
newDecompress (DecompressConfig windowBits dict bufSiz) = do
    zs <- newCPtr'
        (do ps <- throwOOMIfNull create_z_stream
            throwZlibIfMinus_ $ inflate_init2 ps windowBits
            return ps)
        free_z_stream_inflate

    buf <- A.newPinnedPrimArray bufSiz
    bufRef <- newIORef buf
    set_avail_out zs buf bufSiz

    let newOutBuffer = do
            buf' <- A.newPinnedPrimArray bufSiz
            writeIORef bufRef buf'
            set_avail_out zs buf' bufSiz

    return (ZStream zs, \ k mbs -> case mbs of
        Just bs -> do
            set_avail_in zs bs (V.length bs)

            let loop = do
                    oavail :: CUInt <- withCPtr zs $ \ ps -> do
                        r <- throwZlibIfMinus (inflate ps (#const Z_NO_FLUSH))
                        when (r == (#const Z_NEED_DICT)) $
                            if V.null dict
                            then throwIO (ZlibException "Z_NEED_DICT" callStack)
                            else do
                                throwZlibIfMinus_ . withPrimVectorUnsafe dict $ \ pdict off len ->
                                    inflate_set_dictionary ps pdict off (fromIntegral len)
                                throwZlibIfMinus_ (inflate ps (#const Z_NO_FLUSH))
                        (#peek struct z_stream_s, avail_out) ps

                    when (oavail == 0) $ do
                        oarr <- A.unsafeFreezeArr =<< readIORef bufRef
                        k (Just (V.PrimVector oarr 0 bufSiz))
                        newOutBuffer
                        loop
            loop

        _ -> 
            let loop = do
                    (r, osiz) <- withCPtr zs $ \ ps -> do
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
                        oarr <- A.unsafeFreezeArr =<< readIORef bufRef
                        k (Just (V.PrimVector oarr 0 osiz))
                        newOutBuffer
                        loop
                    else do
                        -- stream ends
                        when (osiz /= 0) $ do
                            oarr <- A.unsafeFreezeArr =<< readIORef bufRef
                            k (Just (V.PrimVector oarr 0 osiz))
                        k EOF
            in loop)

-- | Reset decompressor's state so that related 'BIO' can be reused.
decompressReset :: ZStream -> IO ()
decompressReset (ZStream fp) = do
    throwZlibIfMinus_ (withCPtr fp inflateReset)

-- | Decompress some bytes.
decompress :: HasCallStack => DecompressConfig -> V.Bytes -> V.Bytes
decompress conf = V.concat . unsafeRunBlock (snd <$> newDecompress conf)

-- | Decompress some bytes in blocks.
decompressBlocks :: HasCallStack => DecompressConfig -> [V.Bytes] -> [V.Bytes]
decompressBlocks conf = unsafeRunBlocks (snd <$> newDecompress conf)

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
data ZlibException = ZlibException CBytes CallStack deriving Show
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

set_avail_in :: CPtr ZStream -> V.Bytes -> Int -> IO ()
set_avail_in zs buf buflen = do
    withPrimVectorSafe buf $ \ pbuf _ ->
        withCPtr zs $ \ ps -> do
            (#poke struct z_stream_s, next_in) ps pbuf
            (#poke struct z_stream_s, avail_in) ps (fromIntegral buflen :: CUInt)

set_avail_out :: CPtr ZStream -> MutablePrimArray RealWorld Word8 -> Int -> IO ()
set_avail_out zs buf bufSiz = do
    withMutablePrimArrayContents buf $ \ pbuf ->
        withCPtr zs $ \ ps -> do
            (#poke struct z_stream_s, next_out) ps pbuf
            (#poke struct z_stream_s, avail_out) ps (fromIntegral bufSiz :: CUInt)
