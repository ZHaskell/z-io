{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-|
Module      : Z.IO.BIO.Concurrent
Description : Base64 codec
Copyright   : (c) Dong Han, 2017-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides some concurrent 'BIO' node, to ease the implementation of producer-consumer model.
All sources and sinks return by this module are safe to be used in multiple threads.

  * Use 'newTQueuePair' for common cases.
  * Use 'newTBQueuePair' if you have a fast producer and you don't want input get piled up in memory.
  * Use 'newBroadcastTChanPair' if you want messages get broadcasted, i.e. every message written by
    producers will be received by every consumers.

It's important to correctly set the numebr of producers, internally it keeps a counter on how many producers
reached their ends, and send EOF to all consumers when last producer ends. So it's a good idea to catch
exceptions and pull the sink(which indicate EOF) on producer side.

@
(sink, src) <- newTQueuePair 2  -- it's important to correctly set the numebr of producers

--------------------------------------------------------------------------------
-- producers

forkIO $ do
    ...
    push x sink             -- producer using push
    ...
    pull sink               -- when EOF is reached, manually pull, you may consider put it in a bracket.

forkIO $ do
    ...
    (runBIO $ ... . sink) -- producer using BIO
        `onException` (pull sink)

--------------------------------------------------------------------------------
-- consumers

forkIO $ do
    ...
    r <- pull src           -- consumer using pull
    case r of Just r' -> ...
              _ -> ...      -- EOF indicate all producers reached EOF

forkIO $ do
    ...
    runBIO $ src . ...    -- consumer using BIO
@

-}

module Z.IO.BIO.Concurrent where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|),(:|>)))
import GHC.Natural
import Z.IO.BIO.Base
import Z.Data.PrimRef
import Z.IO.Exception

-- | Zip two BIO node by running them concurrently.
--
-- This implementation use 'MVar' to synchronize two BIO's output, which has some implications:
--
--   * Two node should output same numebr of results.
--   * If the number differs, one node maybe
--
zip :: BIO a b -> BIO a c -> BIO a (b,c)
{-# INLINABLE zip #-}
zip b1 b2 = \ k mx -> do
    bEOF <- newTVarIO False
    cEOF <- newTVarIO False
    bBuf <- newTVarIO Seq.empty
    cBuf <- newTVarIO Seq.empty
    _ <- forkIO (b1 (f bBuf bEOF) mx)
    _ <- forkIO (b2 (f cBuf cEOF) mx)
    loop k bBuf cBuf bEOF cEOF
  where
    f xBuf xEOF = \ mx ->
        case mx of
            Just x -> atomically $ modifyTVar' xBuf (:|> x)
            _ -> atomically $ writeTVar xEOF True

    loop k bBuf cBuf bEOF cEOF = join . atomically $ do
        bs <- readTVar bBuf
        cs <- readTVar cBuf
        beof <- readTVar bEOF
        ceof <- readTVar cEOF
        case bs of
            b :<| bs' -> case cs of
                c :<| cs' -> do
                    writeTVar bBuf bs'
                    writeTVar cBuf cs'
                    return (k (Just (b, c)) >> loop k bBuf cBuf bEOF cEOF)
                _ -> if ceof then return (k EOF) else retry
            _ -> if beof then return (k EOF) else retry

-- | Make an unbounded queue and a pair of sink and souce connected to it.
newTQueuePair :: Int -- ^ number of producers
              -> IO (Sink a, Source a)
{-# INLINABLE newTQueuePair #-}
newTQueuePair n = do
    q <- newTQueueIO
    ec <- newCounter 0
    return
        ( \ k mx -> case mx of
                Just _ -> atomically (writeTQueue q mx)
                _ -> do
                    i <- atomicAddCounter' ec 1
                    when (i == n) $ do
                        atomically (writeTQueue q EOF)
                        k EOF

        , \ k _ ->
            let loop = uninterruptibleMask $ \ restore -> do
                    x <- restore $ atomically (readTQueue q)
                    case x of Just _ -> k x >> loop
                              _ -> do atomically (unGetTQueue q EOF)
                                      k EOF
            in loop)

-- | Make an bounded queue and a pair of sink and souce connected to it.
newTBQueuePair :: Int       -- ^ number of producers
               -> Natural   -- ^ queue buffer bound
               -> IO (Sink a, Source a)
{-# INLINABLE newTBQueuePair #-}
newTBQueuePair n bound = do
    q <- newTBQueueIO bound
    ec <- newCounter 0
    return
        ( \ k mx -> case mx of
                Just _ -> atomically (writeTBQueue q mx)
                _ -> do
                    i <- atomicAddCounter' ec 1
                    when (i == n) $ do
                        atomically (writeTBQueue q EOF)
                        k EOF

        , \ k _ ->
            let loop = uninterruptibleMask $ \ restore -> do
                    x <- restore $ atomically (readTBQueue q)
                    case x of Just _ -> k x >> loop
                              _ -> do atomically (unGetTBQueue q EOF)
                                      k EOF
            in loop)

-- | Make a broadcast chan and a sink connected to it, and a function return sources to receive broadcast message.
newBroadcastTChanPair :: Int                        -- ^ number of producers
                      -> IO (Sink a, IO (Source a)) -- ^ (Sink, IO Source)
{-# INLINABLE newBroadcastTChanPair #-}
newBroadcastTChanPair n = do
    b <- newBroadcastTChanIO
    ec <- newCounter 0
    let dupSrc = do
            c <- atomically (dupTChan b)
            return $ \ k _ ->
                let loop = do
                        x <- atomically (readTChan c)
                        case x of Just _ -> k x >> loop
                                  _ -> k EOF
                in loop

    return
        (\ k mx -> case mx of
            Just _ -> atomically (writeTChan b mx)
            _ -> do i <- atomicAddCounter' ec 1
                    when (i == n) (atomically (writeTChan b EOF))
                    k EOF
       , dupSrc)
