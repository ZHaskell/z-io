{-# LANGUAGE OverloadedStrings #-}

module Z.Compression.ZlibSpec where

import           Control.Monad
import           Data.IORef
import           Data.ByteString       as B
import           Data.ByteString.Lazy  as BL
import           Z.Data.Vector         as V
import           Z.Compression.Zlib
import qualified Codec.Compression.Zlib as TheZlib
import           Z.IO.Buffered
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit


spec :: Spec
spec = describe "zlib" $ do

    describe "decompress . compress === id" . modifyMaxSize (*50) $ do
        prop "decompress . compress === id" $ \ xs  -> do
            decompress defaultDecompressConfig
                (compress defaultCompressConfig (V.pack xs)) === V.pack xs

        prop "decompress . compress === id(with dict)" $ \ xs  -> do
            let dict = "aabbccdd"
            decompress defaultDecompressConfig{decompressDictionary = dict}
                (compress defaultCompressConfig{compressDictionary = dict} (V.pack xs))
                    === V.pack xs

        prop "compress === TheZlib.compress" $ \ xs  -> do
            V.unpack (compress defaultCompressConfig (V.pack xs)) ===
                BL.unpack (TheZlib.compressWith TheZlib.defaultCompressParams (BL.pack xs))

        prop "compress === TheZlib.compress(with dict)" $ \ xs  -> do
            V.unpack (compress defaultCompressConfig{compressDictionary = "aabbccdd"} (V.pack xs)) ===
                BL.unpack (TheZlib.compressWith
                    TheZlib.defaultCompressParams{TheZlib.compressDictionary = Just "aabbccdd" }
                        (BL.pack xs))

        prop "decompressSink . compressSource === id" $ \ xss -> do
            ref <- newIORef []
            (write, flush) <- compressSink defaultCompressConfig
                (\ x -> modifyIORef' ref (x:), return ())

            let vs = Prelude.map V.pack xss

            forM_ vs write
            flush

            source <- sourceFromList . Prelude.reverse =<< readIORef ref
            vs' <- collectSource =<< decompressSource defaultDecompressConfig source

            V.concat vs @=? V.concat vs'

        prop "decompressSink . compressSource === id(with dict)" $ \ xss -> do
            let dict = "aabbccdd"
            ref <- newIORef []
            (write, flush) <- compressSink defaultCompressConfig{compressDictionary = dict}

                (\ x -> modifyIORef' ref (x:), return ())

            let vs = Prelude.map V.pack xss

            forM_ vs write
            flush

            source <- sourceFromList . Prelude.reverse =<< readIORef ref
            vs' <- collectSource =<< decompressSource
                defaultDecompressConfig{decompressDictionary = dict} source

            V.concat vs @=? V.concat vs'

