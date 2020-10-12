{-# LANGUAGE OverloadedStrings #-}
module Z.IO.ProcessSpec where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Z.IO.Process
import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = describe "process" $ do
    it "arguments should be passed" $ do
        (out, err, ecode) <- readProcess defaultProcessOptions{
                processFile = "echo"
            ,   processArgs = ["-n", "hello", "world", "good", "byte"]
            } ""

        assertEqual "echo back arguments" out "hello world good byte"
        assertEqual "echo exit successfully" ecode ExitSuccess

    it "UTF8 input should be passed" $ do
        (out, err, ecode) <- readProcess defaultProcessOptions{
                processFile = "cat"
            } "你好世界再见"

        assertEqual "cat echo back stdin" out "你好世界再见"
        assertEqual "cat exit successfully" ecode ExitSuccess

    it "environment should be passed" $ do
        (out, err, ecode) <- readProcess defaultProcessOptions{
                processFile = "env"
            ,   processEnv = Just [("hello", "world"), ("good", "byte")]
            } ""

        assertEqual "env echo back environment" out "hello=world\ngood=byte\n"
        assertEqual "env exit successfully" ecode ExitSuccess

    it "exit code should be passed" $ do
        (out, err, ecode) <- readProcess defaultProcessOptions{
                processFile = "sh"
            ,   processArgs = ["-c", "exit 8"]
            } ""

        assertEqual "exit code" ecode (ExixFailure 8)

