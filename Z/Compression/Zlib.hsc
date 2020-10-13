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

module Z.Compression.Zlib(
  -- * Compression
    CompressConfig(..)
  , defaultCompressConfig
  , compress
  , compressSink
  , WindowBits
  , defaultWindowBits
  , MemLevel
  , defaultMemLevel
  -- * Decompression
  , DecompressConfig(..)
  , defaultDecompressConfig
  , decompress
  , decompressSource
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
import           Foreign            hiding (void)
import           Foreign.C
import           GHC.Generics
import           System.IO.Unsafe   (unsafePerformIO)
import           Z.Data.Array       as A
import           Z.Data.CBytes      as CBytes
import           Z.Data.Vector.Base as V
import           Z.Data.Text.ShowT  (ShowT)
import           Z.Foreign
import           Z.IO.Buffered
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
    }   deriving (Show, Eq, Ord, Generic)
        deriving anyclass ShowT

defaultCompressConfig :: CompressConfig
defaultCompressConfig =
    CompressConfig Z_DEFAULT_COMPRESSION  defaultWindowBits
        defaultMemLevel V.empty Z_DEFAULT_STRATEGY

-- | Compress all the data written to a output.
--
compressSink :: HasCallStack
           => CompressConfig
           -> Sink V.Bytes
           -> IO (Sink V.Bytes)
compressSink (CompressConfig level windowBits memLevel dict strategy) (write, flush) = do
    zs <- newForeignPtr free_z_stream_deflate =<< create_z_stream
    buf <- A.newPinnedPrimArray bufSiz
    set_avail_out zs buf bufSiz
    bufRef <- newIORef buf

    withForeignPtr zs $ \ ps -> do
        throwZlibIfMinus_ $ deflate_init2 ps level windowBits memLevel strategy
        unless (V.null dict) $
            throwZlibIfMinus_ . withPrimVectorUnsafe dict $ \ pdict off len ->
            deflate_set_dictionary ps pdict off (fromIntegral $ len)

    return (zwrite zs bufRef, zflush zs bufRef)

  where
    bufSiz = V.defaultChunkSize

    zwrite zs bufRef input = do
        set_avail_in zs input (V.length input)
        zloop zs bufRef

    zloop zs bufRef = do
        oavail :: CUInt <- withForeignPtr zs $ \ ps -> do
            throwZlibIfMinus_ (deflate ps (#const Z_NO_FLUSH))
            (#peek struct z_stream_s, avail_out) ps

        when (oavail == 0) $ do
            oarr <- A.unsafeFreezeArr =<< readIORef bufRef
            buf' <- A.newPinnedPrimArray bufSiz
            set_avail_out zs buf' bufSiz
            writeIORef bufRef buf'
            write (V.PrimVector oarr 0 bufSiz)
            zloop zs bufRef

    zflush zs bufRef = do
        r :: CInt <- withForeignPtr zs $ \ ps -> do
            r <- throwZlibIfMinus (deflate ps (#const Z_FINISH))
            oavail :: CUInt <- (#peek struct z_stream_s, avail_out) ps
            when (oavail /= fromIntegral bufSiz) $ do
                oarr <- A.unsafeFreezeArr =<< readIORef bufRef
                write (V.PrimVector oarr 0 (bufSiz - fromIntegral oavail))
                flush
            return r

        when (r /= (#const Z_STREAM_END)) $ do
            buf' <- A.newPinnedPrimArray bufSiz
            set_avail_out zs buf' bufSiz
            writeIORef bufRef buf'
            zflush zs bufRef

-- | Compress some bytes.
compress :: HasCallStack => CompressConfig -> V.Bytes -> V.Bytes
compress conf input = unsafePerformIO $ do
    ref <- newIORef []
    (write, flush) <- compressSink conf (\ x -> modifyIORef' ref (x:), return ())
    write input
    flush
    V.concat . reverse <$> readIORef ref


{-
compressBuilderStream :: HasCallStack
                      => CompressConfig
                      -> (B.Builder a -> IO ())
                      -> IO (B.Builder a -> IO ())


-}

data DecompressConfig = DecompressConfig
    { decompressWindowBits :: WindowBits
    , decompressDictionary :: V.Bytes
    }   deriving (Show, Eq, Ord, Generic)
        deriving anyclass ShowT

defaultDecompressConfig :: DecompressConfig
defaultDecompressConfig = DecompressConfig defaultWindowBits V.empty

-- | Decompress bytes from source.
decompressSource :: DecompressConfig
                 -> Source V.Bytes
                 -> IO (Source V.Bytes)
decompressSource (DecompressConfig windowBits dict) source = do 
    zs <- newForeignPtr free_z_stream_inflate =<< create_z_stream
    buf <- A.newPinnedPrimArray bufSiz
    set_avail_out zs buf bufSiz
    bufRef <- newIORef buf

    withForeignPtr zs $ \ ps -> do
        throwZlibIfMinus_ $ inflate_init2 ps windowBits

    return (zread zs bufRef)
  where
    bufSiz = V.defaultChunkSize
    
    zread zs bufRef = do 
        bufLen <- A.sizeofMutableArr =<< readIORef bufRef 
        if bufLen == 0
        then return Nothing
        else do
            oavail :: CUInt <- withForeignPtr zs (#peek struct z_stream_s, avail_out)
            if (oavail == 0) 
            then do
                oarr <- A.unsafeFreezeArr =<< readIORef bufRef
                buf' <- A.newPinnedPrimArray bufSiz
                set_avail_out zs buf' bufSiz
                writeIORef bufRef buf'
                return (Just (V.PrimVector oarr 0 bufSiz))
            else zloop zs bufRef 

    zloop zs bufRef  = do
        iavail :: CUInt <- withForeignPtr zs (#peek struct z_stream_s, avail_in) 
        if iavail == 0
        then do
            input <- source
            case input of
                Just input' -> do
                    set_avail_in zs input' (V.length input')
                    withForeignPtr zs $ \ ps -> do
                        r <- throwZlibIfMinus (inflate ps (#const Z_NO_FLUSH))
                        when (r == (#const Z_NEED_DICT) && not (V.null dict)) $ do
                            throwZlibIfMinus_ . withPrimVectorUnsafe dict $ \ pdict off len ->
                                inflate_set_dictionary ps pdict off (fromIntegral len)
                    zread zs bufRef 
                _ -> zfinish zs bufRef []
        else do
            withForeignPtr zs $ \ ps ->
                throwZlibIfMinus_ (inflate ps (#const Z_NO_FLUSH))
            zloop zs bufRef 

    zfinish zs bufRef acc = do
        r <- withForeignPtr zs $ \ ps -> do
            throwZlibIfMinus (inflate ps (#const Z_FINISH))

        oavail :: CUInt <- withForeignPtr zs (#peek struct z_stream_s, avail_out) 
        oarr <- A.unsafeFreezeArr =<< readIORef bufRef
        let !v = V.PrimVector oarr 0 (bufSiz - fromIntegral oavail)

        if (r == (#const Z_STREAM_END)) 
        then do
            writeIORef bufRef =<< A.newArr 0
            let !v' = V.concat (reverse (v:acc))
            return (Just v')
        else do
            buf' <- A.newPinnedPrimArray bufSiz
            set_avail_out zs buf' bufSiz
            writeIORef bufRef buf'
            zfinish zs bufRef (v:acc)

    
-- | Decompress some bytes.
decompress :: HasCallStack => DecompressConfig -> V.Bytes -> V.Bytes
decompress conf input = V.concat . unsafePerformIO $ do
     collectSource =<< decompressSource conf =<< sourceFromList [input]

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

data ZlibException = ZlibException CBytes CallStack deriving (Show, Typeable)
instance Exception ZlibException

throwZlibIfMinus :: HasCallStack => IO CInt -> IO CInt
throwZlibIfMinus f = do
    r <- f
    if r < 0 && r /= (#const Z_BUF_ERROR)
    then throwIO (ZlibException (toZErrorMsg r) callStack)
    else return r

throwZlibIfMinus_ :: HasCallStack => IO CInt -> IO ()
throwZlibIfMinus_ = void . throwZlibIfMinus

data ZStream

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
    inflate_init2 :: Ptr ZStream -> WindowBits -> IO CInt

foreign import ccall unsafe
    inflate_set_dictionary :: Ptr ZStream -> BA## Word8 -> Int -> Int -> IO CInt

foreign import ccall unsafe
    inflate :: Ptr ZStream -> CInt -> IO CInt

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
