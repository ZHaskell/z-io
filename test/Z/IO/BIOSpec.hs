{-# LANGUAGE OverloadedStrings #-}

module Z.IO.BIOSpec where

import           Control.Monad
import qualified Codec.Compression.Zlib as TheZlib
import           Data.IORef
import qualified Z.Data.Vector         as V
import           Z.IO.BIO.Zlib
import           Z.IO
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           System.IO.Unsafe


spec :: Spec
spec = describe "BIO" . modifyMaxSize (*10) $ do

    describe "decode . encode === id(Base64)" $
        prop "Base64" $ \ xs ->
            let r = unsafePerformIO $ do
                    src <- sourceFromList xs
                    (rRef, sink) <- sinkToList
                    enc <- newBase64Encoder
                    dec <- newBase64Decoder
                    runBIO $ src >|> enc >|> dec >|> sink
                    readIORef rRef
            in V.concat r === V.concat xs

    describe "decode . encode === id(Hex)" $ do
        prop "Hex" $ \ xs upper ->
            let r = unsafePerformIO $ do
                    src <- sourceFromList xs
                    (rRef, sink) <- sinkToList
                    let enc = hexEncoder upper
                    dec <- newHexDecoder
                    runBIO $ src >|> enc >|> dec >|> sink
                    readIORef rRef
            in V.concat r === V.concat xs

