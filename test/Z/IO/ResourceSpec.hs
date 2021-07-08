{-# LANGUAGE ScopedTypeVariables #-}

module Z.IO.ResourceSpec where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Z.Data.PrimRef
import           Z.IO.Resource          as R
import           Test.Hspec
import           Test.HUnit

data WorkerException = WorkerException deriving (Show)

instance Exception WorkerException

spec :: Spec
spec = describe "resource tests" $ do
    it "resource pool" $ do
        resCounter <- newCounter 0
        workerCounter <- newCounter 0
        let res = initResource (atomicAddCounter_ resCounter 1)
                               (\ _ -> atomicSubCounter_ resCounter 1)
            resPool = initSimplePool res 100 1

        R.withResource resPool $ \ pool -> do
            forM_ [1..200] $ \ k -> forkIO. R.withSimplePool pool $ \ i -> do
                atomicAddCounter_ workerCounter 1
                threadDelay 100000

            threadDelay 1000000

            r <- readPrimRef resCounter
            assertEqual "pool should keep returned resources alive" 100 r

            threadDelay 5000000  -- after 5s, 200 thread should release all resources

            w <- readPrimRef workerCounter
            assertEqual "worker should be able to get resource" 200 w

            r <- readPrimRef resCounter
            assertEqual "pool should reap unused resources" 0 r

            -- Let's test again

            writePrimRef workerCounter 0

            forM_ [1..200] $ \ k -> forkIO. R.withSimplePool pool $ \ i -> do
                atomicAddCounter_ workerCounter 1
                threadDelay 100000

            threadDelay 1000000

            r <- readPrimRef resCounter
            assertEqual "pool should keep returned resources alive" 100 r

            threadDelay 5000000

            w <- readPrimRef workerCounter
            assertEqual "worker should be able to get resource" 200 w

            r <- readPrimRef resCounter
            assertEqual "pool should reap unused resources" 0 r

    it "resource pool under exceptions" $ do
        resCounter <- newCounter 0
        let res = initResource (atomicAddCounter' resCounter 1)
                               (\ _ -> atomicSubCounter_ resCounter 1)
            resPool = initSimplePool res 100 1
        R.withResource resPool $ \ pool -> do

            forM_ [1..200] $ \ k -> forkIO. R.withSimplePool pool $ \ i -> do
                threadDelay 100000
                when (even i) (throwIO WorkerException)

            threadDelay 1000000

            threadDelay 5000000

            r <- readPrimRef resCounter
            assertEqual "pool should reap unused resources" 0 r
