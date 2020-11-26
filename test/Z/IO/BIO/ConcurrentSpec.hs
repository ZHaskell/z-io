{-# LANGUAGE OverloadedStrings #-}

module Z.IO.BIO.ConcurrentSpec where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Z.IO.BIO.Concurrent
import           Z.IO.BIO
import           Test.QuickCheck
import           Test.QuickCheck.Function
import           Test.QuickCheck.Property
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           System.IO.Unsafe


spec :: Spec
spec = describe "BIO.Concurrent" $ do

    it "TQueueNode works as expected" $ do
        let content = [1..1000]

        (sink, src) <- newTQueueNode 2
        sumRef <- newIORef 0

        let producter = do
                src' <- sourceListWithDelay content
                runBIO (src' >|> sink)

        let consumer =  do
                (rRef, sink') <- sinkToList
                runBIO (src >|> sink')
                r <- readIORef rRef
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
                runBIO (src' >|> sink)

        let consumer =  do
                (rRef, sink') <- sinkToList
                runBIO (src >|> sink')
                r <- readIORef rRef
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
                runBIO (src' >|> sink)

        let consumer =  do
                (rRef, sink') <- sinkToList
                src <- srcf
                runBIO (src >|> sink')
                r <- readIORef rRef
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
sourceListWithDelay xs0 = do
    xsRef <- newIORef xs0
    return BIO{ pull = popper xsRef }
  where
    popper xsRef = do
        xs <- readIORef xsRef
        case xs of
            (x:xs') -> do
                writeIORef xsRef xs'
                threadDelay x
                return (Just x)
            _ -> return Nothing
