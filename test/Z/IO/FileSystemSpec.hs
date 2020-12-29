{-# LANGUAGE OverloadedStrings #-}

module Z.IO.FileSystemSpec where

import           Control.Concurrent.MVar (readMVar)
import           Control.Monad
import           Data.Bits
import           Z.Data.ASCII
import           Z.Data.Vector         as V
import           Z.Data.Vector.Base    as V
import           Data.List               as List
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Z.IO.Buffered
import           Z.IO.Exception
import           Z.IO.FileSystem
import           Z.IO.Resource
import           Z.IO.UV.Manager
import           Test.Hspec
import           Test.HUnit

spec :: Spec
spec = describe "filesystem operations" $ do

        let content = "Hello world!"
            content2 = V.cycleN 1024 "quick fox jumps over the lazy dog, 世界你好!\n"
            size = V.length content
            size2 = V.length content2


        it "create a temp dir" $ do
            tempdir <- mkdtemp "stdio-filesystem-unit"
            dirs <- scandir "./"
            rmdir tempdir
            List.lookup tempdir dirs @?= Just DirEntDir


        let flags = O_RDWR .|. O_CREAT
            mode = DEFAULT_MODE
            filename = "test-file"

        it "Opens and writes a file" $ do
            withResource (initFile filename flags mode) $ \ file -> do
                o <- newBufferedOutput' 4096 file
                writeBuffer o content
                flushBuffer o

            withResource (initFile filename flags mode) $ \ file -> do
                i <- newBufferedInput' 4096 file
                written <- readExactly size i
                written @?= content

                fr <- newFilePtr file 0
                i <- newBufferedInput' 4096 fr
                written <- readExactly size i
                written @=? content


            unlink filename

        it "Opens and writes a file II" $ do
            withResource (initFile filename flags mode) $ \ file -> do
                o <- newBufferedOutput' 4096 file
                writeBuffer o content2
                flushBuffer o

            withResource (initFile filename flags mode) $ \ file -> do
                i <- newBufferedInput' 4096 file
                written <- readExactly size2 i
                written @?= content2

            withResource (initFile filename flags mode) $ \ file -> do
                i <- newBufferedInput' 4096 file
                Just firstLine <- readLine i
                firstLine  @?= fst (V.break (== c2w '\n') content2)

                fr <- newFilePtr file (fromIntegral $ size2 `div` 2)
                i <- newBufferedInput' 4096 fr
                replicateM_ 512 $ do
                    Just firstLine <- readLine i
                    firstLine  @=? fst (V.break (== c2w '\n') content2)

            unlink filename


        it "create and remove dir" $ do
            tempdir <- mkdtemp "stdio-filesystem-unit"
            let dirname  = tempdir <> "/test-dir"
            mkdir dirname mode
            dirs <- scandir tempdir
            print dirs
            rmdir dirname
            rmdir tempdir
            List.lookup "test-dir" dirs @?= Just DirEntDir

        let linkname  = "test-link"
            symlinkname  = "test-symlink"
            symlinkname2  = "test-symlink2"

        it "link stat should be equal to target file" $ ( do

            withResource (initFile filename flags mode) $ \ file -> return ()

            s0 <- stat filename

            link filename linkname
            symlink "test-link" symlinkname SYMLINK_DEFAULT

            absfp <- realpath filename
            symlink absfp symlinkname2 SYMLINK_DEFAULT  -- the second way to create a proper symlink

            s1 <- stat linkname
            s2 <- stat symlinkname
            s2' <- stat symlinkname2

            s0 @=? s1 {stNlink = 1, stCtim = stCtim s0} -- update hard link number, stCtim could have some small diff
            s0 @=? s2 {stNlink = 1, stCtim = stCtim s0}
            s0 @=? s2' {stNlink = 1, stCtim = stCtim s0}

            withResource (initFile filename flags mode) $ \ file -> do
                s4 <- fstat file
                s0 @=? s4 {stNlink = 1, stCtim = stCtim s0}
            ) `finally` ( do
                unlink filename
                unlink linkname
                unlink symlinkname
                unlink symlinkname2
            )

        it "utime result in stat change" $ do
            withResource (initFile filename flags mode) $ \ file -> return ()
            utime filename 1000.2000 3000.4000
            s <- stat filename
            print s
            uvtSecond (stAtim s) @?= 1000
            uvtNanoSecond (stAtim s) @?= 200000000
            uvtSecond (stMtim s) @?= 3000
            uvtNanoSecond (stMtim s) @?= 400000000
            unlink filename

        it "futime result in fstat change" $ do
            withResource (initFile filename flags mode) $ \ file -> do
                futime file 5000.6000 7000.8000
                s <- fstat file
                print s
                uvtSecond (stAtim s) @?= 5000
                uvtNanoSecond (stAtim s) @?= 600000000
                uvtSecond (stMtim s) @?= 7000
                uvtNanoSecond (stMtim s) @?= 800000000
            unlink filename

