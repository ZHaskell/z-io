{-# LANGUAGE OverloadedStrings #-}
module Z.IO.ProcessSpec where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Z.IO.Buffered
import           Z.IO.Process
import           Z.IO.Resource
import           Z.IO.FileSystem
import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = describe "process" $ do
    it "arguments should be passed" $ do
        (out, err, ecode) <- readProcess defaultProcessOptions{
                processFile = "echo"
            ,   processArgs = ["-n", "hello", "world", "good", "byte"]
            } ""

        assertEqual "echo back arguments" "hello world good byte" out
        assertEqual "echo exit successfully" ExitSuccess ecode

    it "UTF8 input should be passed" $ do
        (out, err, ecode) <- readProcess defaultProcessOptions{
                processFile = "cat"
            } "你好世界再见"

        assertEqual "cat echo back stdin" "你好世界再见" out
        assertEqual "cat exit successfully" ExitSuccess ecode

    it "environment should be passed" $ do
        (out, err, ecode) <- readProcess defaultProcessOptions{
                processFile = "env"
            ,   processEnv = Just [("hello", "world"), ("good", "byte")]
            } ""

        assertEqual "env echo back environment" "hello=world\ngood=byte\n" out
        assertEqual "env exit successfully" ExitSuccess ecode

    it "exit code should be passed" $ do
        (out, err, ecode) <- readProcess defaultProcessOptions{
                processFile = "sh"
            ,   processArgs = ["-c", "exit 8"]
            } ""

        assertEqual "exit code" (ExitFailure 8) ecode

    it "redirect stdin, stdout to file" $ do

        tempdir <- mkdtemp "stdio-filesystem-unit"
        let ifilename = tempdir <> "/test-stdin"
            ofilename = tempdir <> "/test-stdout"
        withResource (initFile ifilename (O_RDWR .|. O_CREAT) DEFAULT_MODE) $ \ input -> do
            bi <- newBufferedOutput' 4096 input
            writeBuffer bi "hello world" >> flushBuffer bi

        ecode <- withResource (initFile ifilename O_RDWR DEFAULT_MODE) $ \ input -> do

            withResource (initFile ofilename (O_RDWR .|. O_CREAT) DEFAULT_MODE) $ \ output -> do

                iF <- getFileFD input
                oF <- getFileFD output

                withResource (initProcess defaultProcessOptions{
                        processFile = "cat"
                    ,   processStdStreams = (ProcessInherit iF, ProcessInherit oF, ProcessIgnore)
                    }) $ \ (_, _, _, pstate) -> do

                        waitProcessExit pstate

        withResource (initFile ofilename (O_RDWR .|. O_CREAT) DEFAULT_MODE) $ \ output -> do
            bo <- newBufferedInput' 4096 output
            o <- readBuffer bo
            assertEqual "cat echo back" "hello world" o

        assertEqual "exit code" ecode ExitSuccess

        -- clean up file
        unlink ifilename
        unlink ofilename
        rmdir tempdir
