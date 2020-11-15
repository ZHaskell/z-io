{-|
Module      : Z.IO.BIO.Concurrent
Description : Base64 codec
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides some concurrent 'BIO' node.
-}

module Z.IO.BIO.Concurrent where

import Control.Concurrent.STM
import GHC.Natural
import Z.IO.BIO

newTQueueNode :: IO (Sink a, Source a)
newTQueueNode = do
    q <- newTQueueIO
    return
        ( BIO (\ x -> atomically (writeTQueue q (Just x)) >> return Nothing)
                   (atomically (writeTQueue q Nothing) >> return Nothing)
        , BIO { pull = do
                    x <- atomically (readTQueue q)
                    case x of Just _ -> return x
                              _ -> do atomically (unGetTQueue q Nothing)
                                      return Nothing})


newTBQueueNode :: Natural -> IO (Sink a, Source a)
newTBQueueNode bound = do
    q <- newTBQueueIO bound
    return
        ( BIO (\ x -> atomically (writeTBQueue q (Just x)) >> return Nothing)
                   (atomically (writeTBQueue q Nothing) >> return Nothing)
        , BIO { pull = do
                    x <- atomically (readTBQueue q)
                    case x of Just _ -> return x
                              _ -> do atomically (unGetTBQueue q Nothing)
                                      return Nothing})

newBroadcastTChanNode :: IO (Sink a, IO (Source a))    -- ^ (The broadcast TChan, Sink, IO Source)
newBroadcastTChanNode = do
    b <- newBroadcastTChanIO
    let dupSrc = do
            c <- atomically (dupTChan b)
            return (BIO { pull = atomically (readTChan c) })
    return ( BIO (\ x -> atomically (writeTChan b (Just x)) >> return Nothing)
                   (atomically (writeTChan b Nothing) >> return Nothing)
           , dupSrc)
