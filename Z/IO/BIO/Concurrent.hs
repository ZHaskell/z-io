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

  * Use 'newTQueueNode' for common cases.
  * Use 'newTBQueueNode' if you have a fast producer and you don't want input get piled up in memory.
  * Use 'newBroadcastTChanNode' if you want messages get broadcasted, i.e. every message written by
    producers will be received by every consumers.

It's important to correctly set the numebr of producers, internally it keeps a counter on how many producers
reached their ends, and send EOF to all consumers when last producer ends. So it's a good idea to catch
exceptions and pull the sink(which indicate EOF) on producer side.

@
(sink, src) <- newTQueueNode 2  -- it's important to correctly set the numebr of producers

forkIO $ do
    ...
    push x sink             -- producer using push
    ...
    pull sink               -- when EOF is reached, manually pull, you may consider put it in a bracket.

forkIO $ do
    ...
    (runBIO $ ... >|> sink) -- producer using BIO
        `onException` (pull sink)
forkIO $ do
    ...
    r <- pull src           -- consumer using pull
    case r of Just r' -> ...
              _ -> ...      -- Nothing indicate all producers reached EOF

forkIO $ do
    ...
    runBIO $ src >|> ...    -- consumer using BIO
@

-}

module Z.IO.BIO.Concurrent where

import Control.Monad
import Control.Concurrent.STM
import GHC.Natural
import Z.IO.BIO
import Z.Data.PrimRef
import Z.IO.Exception

-- | Make an unbounded queue and a pair of sink and souce connected to it.
newTQueueNode :: Int -- ^ number of producers
              -> IO (Sink a, Source a)
newTQueueNode n = do
    q <- newTQueueIO
    ec <- newCounter 0
    return
        ( BIO (\ x -> atomically (writeTQueue q (Just x)) >> return Nothing)
                (do i <- atomicAddCounter' ec 1
                    when (i == n) (atomically (writeTQueue q Nothing))
                    return Nothing)
        , BIO { pull = uninterruptibleMask $ \ restore -> do
                    x <- restore $ atomically (readTQueue q)
                    case x of Just _ -> return x
                              _ -> do atomically (unGetTQueue q Nothing)
                                      return Nothing})

-- | Make an bounded queue and a pair of sink and souce connected to it.
newTBQueueNode :: Int       -- ^ number of producers
               -> Natural   -- ^ queue buffer bound
               -> IO (Sink a, Source a)
newTBQueueNode n bound = do
    q <- newTBQueueIO bound
    ec <- newCounter 0
    return
        ( BIO (\ x -> atomically (writeTBQueue q (Just x)) >> return Nothing)
                (do i <- atomicAddCounter' ec 1
                    when (i == n) (atomically (writeTBQueue q Nothing))
                    return Nothing)
        , BIO { pull = uninterruptibleMask $ \ restore -> do
                    x <- restore $ atomically (readTBQueue q)
                    case x of Just _ -> return x
                              _      -> do atomically (unGetTBQueue q Nothing)
                                           return Nothing})

-- | Make a broadcast chan and a sink connected to it, and a function return sources to receive broadcast message.
newBroadcastTChanNode :: Int                        -- ^ number of producers
                      -> IO (Sink a, IO (Source a)) -- ^ (Sink, IO Source)
newBroadcastTChanNode n = do
    b <- newBroadcastTChanIO
    ec <- newCounter 0
    let dupSrc = do
            c <- atomically (dupTChan b)
            return (BIO { pull = do
                            x <- atomically (readTChan c)
                            case x of Just _ -> return x
                                      _ -> return Nothing })
    return ( BIO (\ x -> atomically (writeTChan b (Just x)) >> return Nothing)
                    (do i <- atomicAddCounter' ec 1
                        when (i == n) (atomically (writeTChan b Nothing))
                        return Nothing)
           , dupSrc)
