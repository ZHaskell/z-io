{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Simple and fast word counter

inspired by https://chrispenner.ca/posts/wc
test file: https://github.com/ChrisPenner/wc/blob/master/data/big.txt

@
 time cat big.txt | wc
  128457 1095695 6488666
 cat big.txt  0.00s user 0.00s system 14% cpu 0.045 total
 wc  0.04s user 0.00s system 99% cpu 0.044 total

time cat big.txt | ./dist-newstyle/build/x86_64-linux/ghc-8.10.2/examples-0.1.0.0/x/wc/build/wc/wc
(6488666,1095695,128457)
cat big.txt  0.00s user 0.01s system 14% cpu 0.046 total
  0.04s user 0.01s system 99% cpu 0.046 total
@

-}
module Main where

import Control.Concurrent
import Z.IO.Buffered
import Z.IO.StdStream
import qualified Z.Data.Vector as V

main :: IO ()
main = do
    printStd =<< withMVar stdinBuf (loop 0 0 0)
    putStd "\n"
  where
    loop :: Int -> Int -> Int -> BufferedInput StdStream -> IO (Int, Int, Int)
    loop !len !wc !lc input = do
        line <- readLine input
        case line of
            Just line' ->
                loop (len + V.length line' + 1)     -- line' has no linefeed, so we add back
                     (wc + length (V.words line'))
                     (lc+1) input
            _ -> return (len, wc, lc)
