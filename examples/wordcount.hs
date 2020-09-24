{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent
import Z.IO.Buffered
import Z.IO.StdStream
import qualified Z.Data.Vector as V

main :: IO ()
main = do
    printStd =<< withMVar stdinBuf (loop 0 0 0)
  where
    loop :: Int -> Int -> Int -> BufferedInput StdStream -> IO (Int, Int, Int)
    loop !len !wc !lc input = do
        line <- readLine input
        print line
        if V.null line
        then return (len, wc, lc)
        else loop (len + V.length line) (wc + length (V.words line)) (lc+1) input
