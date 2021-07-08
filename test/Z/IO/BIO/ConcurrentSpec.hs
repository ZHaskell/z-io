{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Z.IO.BIO.ConcurrentSpec where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import qualified Z.IO.BIO               as BIO
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.QuickCheck.Monadic as QM
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           System.IO.Unsafe


spec :: Spec
spec = describe "BIO.Concurrent" $ do

    prop "zip works like zip on Sources" $ \ xs ys -> monadicIO $ do
        (zs :: [(Int, Int)]) <- liftIO . BIO.run $ BIO.zip  (BIO.sourceFromList xs) (BIO.sourceFromList ys)
        QM.assert (zs == zip xs ys)

    it "TQueueNode works as expected" $ do
        let content = [1..1000]

        (sink, src) <- BIO.newTQueuePair 2
        sumRef <- newIORef 0

        let producter = do
                src' <- sourceListWithDelay content
                BIO.run_ (src' . sink)

        let consumer =  do
                (rRef, sink') <- BIO.sinkToList
                BIO.run_ (src . sink')
                r <- takeMVar rRef
                atomicModifyIORef' sumRef $ \ x -> (x + sum r, ())

        forkIO $ consumer
        forkIO $ consumer
        forkIO $ consumer
        forkIO $ producter
        forkIO $ producter

        threadDelay 10000000

        s <- readIORef sumRef
        s @?= (sum content * 2)

    it "TBQueueNode works as expected" $ do
        let content = [1..1000]

        (sink, src) <- BIO.newTBQueuePair 2 10
        sumRef <- newIORef 0

        let producter = do
                src' <- sourceListWithDelay content
                BIO.run_ (src' . sink)

        let consumer =  do
                (rRef, sink') <- BIO.sinkToList
                BIO.run_ (src . sink')
                r <- takeMVar rRef
                atomicModifyIORef' sumRef $ \ x -> (x + sum r, ())

        forkIO $ consumer
        forkIO $ consumer
        forkIO $ consumer
        forkIO $ producter
        forkIO $ producter

        threadDelay 10000000

        s <- readIORef sumRef
        s @?= (sum content * 2)

    it "TChanNode works as expected" $ do
        let content = [1..1000]

        (sink, srcf) <- BIO.newBroadcastTChanPair 2
        sumRef <- newIORef 0

        let producter = do
                src' <- sourceListWithDelay content
                BIO.run_ (src' . sink)

        let consumer =  do
                (rRef, sink') <- BIO.sinkToList
                src <- srcf
                BIO.run_ (src . sink')
                r <- takeMVar rRef
                atomicModifyIORef' sumRef $ \ x -> (x + sum r, ())

        forkIO $ consumer
        forkIO $ consumer
        forkIO $ consumer
        forkIO $ producter
        forkIO $ producter

        threadDelay 10000000

        s <- readIORef sumRef
        s @?= (sum content * 2 * 3)


sourceListWithDelay :: [Int] -> IO (BIO.Source Int)
sourceListWithDelay xs = do
    return $ \ k _ -> do
        forM_ xs $ \ x -> do
            threadDelay x
            k (Just x)
        k Nothing
