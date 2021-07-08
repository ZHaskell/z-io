{-# LANGUAGE OverloadedStrings #-}

module Z.IO.BIO.BaseSpec where

import           Control.Concurrent
import           Control.Monad
import qualified Codec.Compression.Zlib as TheZlib
import           Data.IORef
import qualified Z.Data.Vector         as V
import           Z.IO.BIO.Zlib
import           Z.IO.BIO.Base
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
                    let src = sourceFromList xs
                    (rRef, sink) <- sinkToList
                    enc <- newBase64Encoder
                    dec <- newBase64Decoder
                    run_ $ src . enc . dec . sink
                    takeMVar rRef
            in V.concat r === V.concat xs

    describe "decode . encode === id(Hex)" $ do
        prop "Hex" $ \ xs upper ->
            let r = unsafePerformIO $ do
                    let src = sourceFromList xs
                    (rRef, sink) <- sinkToList
                    dec <- newHexDecoder
                    run_ $ src . hexEncode upper . dec . sink
                    takeMVar rRef
            in V.concat r === V.concat xs

