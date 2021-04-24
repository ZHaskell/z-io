{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Z.IO.BIO.ConcurrentSpec where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Z.IO.BIO.Concurrent
import           Z.IO.BIO
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

    prop "zipBIO works like zip on Sources" $ \ xs ys -> monadicIO $ do
        (zs :: [(Int, Int)]) <- liftIO . runBIO $ zipBIO  (sourceFromList xs) (sourceFromList ys)
        QM.assert (zs == zip xs ys)

    it "TQueueNode works as expected" $ do
        let content = [1..1000]

        (sink, src) <- newTQueueNode 2
        sumRef <- newIORef 0

        let producter = do
                src' <- sourceListWithDelay content
                runBIO_ (src' . sink)

        let consumer =  do
                (rRef, sink') <- sinkToList
                runBIO_ (src . sink')
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

        (sink, src) <- newTBQueueNode 2 10
        sumRef <- newIORef 0

        let producter = do
                src' <- sourceListWithDelay content
                runBIO_ (src' . sink)

        let consumer =  do
                (rRef, sink') <- sinkToList
                runBIO_ (src . sink')
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

        (sink, srcf) <- newBroadcastTChanNode 2
        sumRef <- newIORef 0

        let producter = do
                src' <- sourceListWithDelay content
                runBIO_ (src' . sink)

        let consumer =  do
                (rRef, sink') <- sinkToList
                src <- srcf
                runBIO_ (src . sink')
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


sourceListWithDelay :: [Int] -> IO (Source Int)
sourceListWithDelay xs = do
    return $ \ k _ -> do
        forM_ xs $ \ x -> do
            threadDelay x
            k (Just x)
        k Nothing
