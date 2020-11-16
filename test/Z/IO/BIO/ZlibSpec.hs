{-# LANGUAGE OverloadedStrings #-}

module Z.IO.BIO.ZlibSpec where

import           Control.Monad
import qualified Codec.Compression.Zlib as TheZlib
import           Data.IORef
import           Data.ByteString       as B
import           Data.ByteString.Lazy  as BL
import           Z.Data.Vector         as V
import           Z.IO.BIO.Zlib
import           Z.IO
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit


spec :: Spec
spec = describe "zlib" $ do

    describe "decompress . compress === id" . modifyMaxSize (*10) $ do
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

        prop "TheZlib.decompress . TheZlib.compress == id(performace benchmark)" $ \ xss -> do
            let vs = Prelude.map B.pack xss
                vs' = TheZlib.decompress . TheZlib.compress $ BL.fromChunks vs

            B.concat vs @=? BL.toStrict vs'

        prop "compress >|> decompress" $ \ xss -> do
            c <- newCompress defaultCompressConfig
            d <- newDecompress defaultDecompressConfig

            let vs = Prelude.map V.pack xss
            vs' <- runBlocks (c >|> d) vs

            V.concat vs @=? V.concat vs'

        prop "compress >|> decompress (with dict)" $ \ xss -> do
            let dict = "aabbccdd"

            c <- newCompress defaultCompressConfig{compressDictionary = dict}
            d <- newDecompress defaultDecompressConfig{decompressDictionary = dict}

            let vs = Prelude.map V.pack xss
            vs' <- runBlocks (c >|> d) vs

            V.concat vs @=? V.concat vs'
