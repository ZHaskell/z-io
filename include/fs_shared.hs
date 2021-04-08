-- This file should be included from both base and threaded FS module

-- | File bundled with offset.
--
-- Reading or writing using 'Input' \/ 'Output' instance will automatically increase offset.
-- 'FilePtr' and its operations are NOT thread safe, use 'MVar' 'FilePtr' in multiple threads.
--
-- The notes on linux 'writeFileP' applied to 'FilePtr' too.
data FilePtr = FilePtr {-# UNPACK #-} !File
                       {-# UNPACK #-} !(PrimIORef Int64)

-- |  Create a file offset bundle from an 'File'.
--
newFilePtr :: File       -- ^ the file we're reading
           -> Int64      -- ^ initial offset
           -> IO FilePtr
newFilePtr uvf off = FilePtr uvf <$> newPrimIORef off

-- | Get current offset.
getFilePtrOffset :: FilePtr -> IO Int64
getFilePtrOffset (FilePtr _ offsetRef) = readPrimIORef offsetRef

-- | Change current offset.
setFilePtrOffset :: FilePtr -> Int64 -> IO ()
setFilePtrOffset (FilePtr _ offsetRef) = writePrimIORef offsetRef

instance Input FilePtr where
    readInput (FilePtr file offsetRef) buf bufSiz =
        readPrimIORef offsetRef >>= \ off -> do
            l <- readFileP file buf bufSiz off
            writePrimIORef offsetRef (off + fromIntegral l)
            return l

instance Output FilePtr where
    writeOutput (FilePtr file offsetRef) buf bufSiz =
        readPrimIORef offsetRef >>= \ off -> do
            writeFileP file buf bufSiz off
            writePrimIORef offsetRef (off + fromIntegral bufSiz)

-- | Quickly open a file and read its content.
readFile :: HasCallStack => CBytes -> IO V.Bytes
readFile filename = do
    withResource (initFile filename O_RDONLY DEFAULT_FILE_MODE) $ \ file -> do
        readAll' =<< newBufferedInput file

-- | Quickly open a file and read its content as UTF8 text.
readTextFile :: HasCallStack => CBytes -> IO T.Text
readTextFile filename = T.validate <$> readFile filename

-- | Quickly open a file and write some content.
writeFile :: HasCallStack => CBytes -> V.Bytes -> IO ()
writeFile filename content = do
    withResource (initFile filename (O_WRONLY .|. O_CREAT) DEFAULT_FILE_MODE) $ \ file -> do
        withPrimVectorSafe content (writeOutput file)

-- | Quickly open a file and write some content as UTF8 text.
writeTextFile :: HasCallStack => CBytes -> T.Text -> IO ()
writeTextFile filename content = writeFile filename (T.getUTF8Bytes content)

-- | Quickly open a file and read its content as a JSON value.
-- Throw 'OtherError' with name @EPARSE@ if JSON value is not parsed.
readJSONFile :: (HasCallStack, JSON.JSON a) => CBytes -> IO a
readJSONFile filename = unwrap "EPARSE" . JSON.decode' =<< readFile filename

-- | Quickly open a file and write a JSON Value.
writeJSONFile :: (HasCallStack, JSON.JSON a) => CBytes -> a -> IO ()
writeJSONFile filename x = writeFile filename (JSON.encode x)

--------------------------------------------------------------------------------

-- | Find all files and directories within a given directory with a predicator.
--
-- @
--  import Z.IO.FileSystem.FilePath (splitExtension)
--  -- find all haskell source file within current dir
--  scandirRecursively "."  (\\ p _ -> (== ".hs") . snd \<$\> splitExtension p)
-- @
scandirRecursively :: HasCallStack => CBytes -> (CBytes -> DirEntType -> IO Bool) -> IO [CBytes]
scandirRecursively dir p = loop [] =<< P.normalize dir
  where
    loop acc0 pdir =
        foldM (\ acc (d,t) -> do
            d' <- pdir `P.join` d
            r <- p d' t
            let acc' = if r then (d':acc) else acc
            if (t == DirEntDir)
            then loop acc' d'
            else return acc'
        ) acc0 =<< scandir pdir

--------------------------------------------------------------------------------

-- | Does given path exist?
--
doesPathExist :: CBytes -> IO Bool
doesPathExist path = maybe False (const True) <$> stat' path

-- | Returns 'True' if the argument file exists and is either a file or a
-- symbolic link to a file, and 'False' otherwise.
doesFileExist :: CBytes -> IO Bool
doesFileExist path = maybe False isFileSt <$> stat' path

-- | Returns 'True' if the argument directory exists and is either a directory or a
-- symbolic link to a directory, and 'False' otherwise.
doesDirExist :: CBytes -> IO Bool
doesDirExist path = maybe False isDirSt <$> stat' path

--------------------------------------------------------------------------------

-- | If given path is a symbolic link?
isLink :: HasCallStack => CBytes -> IO Bool
isLink = fmap isLinkSt . lstat

-- | If given path is a directory or a symbolic link to a directory?
isDir :: HasCallStack => CBytes -> IO Bool
isDir = fmap isDirSt . stat

-- | If given path is a file or a symbolic link to a file?
isFile :: HasCallStack => CBytes -> IO Bool
isFile = fmap isFileSt . stat

-- | Shortcut to @\\ st -> stMode st .&. S_IFMT == S_IFLNK@
--
-- Note you should use 'lstat' to get the link's stat.
isLinkSt :: FStat -> Bool
isLinkSt st = stMode st .&. S_IFMT == S_IFLNK

-- | Shortcut to @\\ st -> stMode st .&. S_IFMT == S_IFDIR@
isDirSt :: FStat -> Bool
isDirSt st = stMode st .&. S_IFMT == S_IFDIR

-- | Shortcut to @\\ st -> stMode st .&. S_IFMT == S_IFREG@
isFileSt :: FStat -> Bool
isFileSt st = stMode st .&. S_IFMT == S_IFREG

-------------------------------------------------------------------------------

-- | Make a temporary file under system 'Env.getTempDir' and automatically clean after used.
--
-- >>> withResource (initTempFile "foo") $ printStd
-- File 13
--
initTempFile :: CBytes -> Resource File
initTempFile prefix =
    initResource initAction unlink >>= (\f -> initFile f O_RDWR DEFAULT_FILE_MODE)
    where
        initAction = Env.getTempDir >>= (`P.join` prefix) >>= mkstemp

-- | Make a temporary directory under system 'Env.getTempDir' and automatically clean after used.
--
-- >>> withResource (initTempDir "foo") $ printStd
-- "/tmp/fooxfWR0L"
--
initTempDir :: CBytes -> Resource CBytes
initTempDir prefix =
    initResource (Env.getTempDir >>= (`P.join` prefix) >>= mkdtemp) rmrf
