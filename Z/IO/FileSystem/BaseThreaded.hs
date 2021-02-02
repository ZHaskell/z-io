{-|
Module      : Z.IO.FileSystem.Threaded
Description : Filesystem IO using threadpool
Copyright   : (c) Dong Han, 2017-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides filesystem API exactly same with `Z.IO.FileSystem`, operations(except 'seek') are implemented using libuv's threadpool to achieve non-block behavior (non-block here meaning won't block other haskell threads), which would be prefered when the operations' estimated time is long(>1ms) or running with a non-threaded haskell runtime, such as accessing network filesystem or scan a very large directory. Otherwise you may block RTS's capability thus all the other haskell threads live on it.

The threadpool version operations have overheads similar to safe FFI, but provide same adventages:

  * The libuv's threadpool have a limit on concurrent threads number (4 by default), which can reduce disk contention.
  * The threadpool version works with non-threaded runtime, which doesn't have safe FFI available.
  * The threadpool version won't relinquish current HEC (Haskell Execution Context) a.k.a. capability.

Most of the time you don't need this module though, since modern hardware(SSD) are sufficiently fast.

-}

module Z.IO.FileSystem.BaseThreaded
  ( -- * regular file devices
    File, initFile, readFileP, writeFileP, getFileFD, seek
  , readFile, readTextFile, writeFile, writeTextFile
  , readJSONFile, writeJSONFile
    -- * file offset bundle
  , FilePtrT, newFilePtrT, getFilePtrOffset, setFilePtrOffset
  -- * filesystem operations
  , mkdir, mkdirp
  , unlink
  , mkdtemp
  , mkstemp
  , rmdir, rmdirrf
  , DirEntType(..)
  , scandir
  , scandirRecursively
  , FStat(..), UVTimeSpec(..)
  , stat, lstat, fstat
  , isLink, isDir, isFile
  , rename
  , fsync, fdatasync
  , ftruncate
  , copyfile
  , AccessResult(..)
  , access
  , chmod, fchmod
  , utime, futime, lutime
  , link, symlink
  , readlink, realpath
  , chown, fchown, lchown
  -- * opening constant
  -- ** AccessMode
  , AccessMode
  , pattern F_OK
  , pattern R_OK
  , pattern W_OK
  , pattern X_OK
  -- ** FileMode
  , FileMode
  , pattern DEFAULT_MODE
  , pattern S_IRWXU
  , pattern S_IRUSR
  , pattern S_IWUSR
  , pattern S_IXUSR
  , pattern S_IRWXG
  , pattern S_IRGRP
  , pattern S_IWGRP
  , pattern S_IXGRP
  , pattern S_IRWXO
  , pattern S_IROTH
  -- ** file type constant
  , pattern S_IFMT
  , pattern S_IFLNK
  , pattern S_IFDIR
  , pattern S_IFREG
  -- ** FileFlag
  , FileFlag
  , pattern O_APPEND
  , pattern O_CREAT
  , pattern O_DIRECT
  , pattern O_DSYNC
  , pattern O_EXCL
  , pattern O_EXLOCK
  , pattern O_NOATIME
  , pattern O_NOFOLLOW
  , pattern O_RDONLY
  , pattern O_RDWR
  , pattern O_SYMLINK
  , pattern O_SYNC
  , pattern O_TRUNC
  , pattern O_WRONLY
  , pattern O_RANDOM
  , pattern O_SHORT_LIVED
  , pattern O_SEQUENTIAL
  , pattern O_TEMPORARY
  -- ** CopyFileFlag
  , CopyFileFlag
  , pattern COPYFILE_DEFAULT
  , pattern COPYFILE_EXCL
  , pattern COPYFILE_FICLONE
  , pattern COPYFILE_FICLONE_FORCE
  -- ** SymlinkFlag
  , SymlinkFlag
  , pattern SYMLINK_DEFAULT
  , pattern SYMLINK_DIR
  , pattern SYMLINK_JUNCTION
  ) where

import           Control.Monad
import           Data.Bits
import           Data.Int
import           Data.IORef
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable               (peekElemOff)
import           Foreign.Marshal.Alloc          (allocaBytes)
import           Z.Data.CBytes                  as CBytes
import           Z.Data.PrimRef.PrimIORef
import qualified Z.Data.Text                    as T
import qualified Z.Data.Text.Print              as T
import qualified Z.Data.Vector                  as V
import qualified Z.Data.JSON                    as JSON
import           Z.Foreign
import           Z.IO.Buffered
import           Z.IO.Exception
import qualified Z.IO.FileSystem.FilePath       as P
import           Z.IO.Resource
import           Z.IO.UV.FFI
import           Z.IO.UV.Manager
import           Prelude hiding                 (writeFile, readFile)

--------------------------------------------------------------------------------
-- File

-- | 'File' and its operations are NOT thread safe, use 'MVar' 'File' in multiple threads.
--
-- Note this is a differet data type from "Z.IO.FileSystem" \'s one, the 'Input'
-- and 'Output' instance use thread pool version functions.
--
-- libuv implements read and write method with both implict and explict offset capable.
-- Implict offset interface is provided by 'Input' \/ 'Output' instances.
-- Explict offset interface is provided by 'readFileP' \/ 'writeFileP'.
--
data File = File {-# UNPACK #-} !FD      -- ^ the file
                 {-# UNPACK #-} !(IORef Bool)  -- ^ closed flag

instance Show File where show = T.toString

instance T.Print File where
    toUTF8BuilderP _ (File fd _) = "File " >> T.int fd

-- | Return File fd.
getFileFD :: File -> IO FD
getFileFD (File fd closedRef) = do
    closed <- readIORef closedRef
    if closed then throwECLOSED else return fd

-- | If fd is -1 (closed), throw 'ResourceVanished' ECLOSED.
checkFileClosed :: HasCallStack => File -> (FD -> IO a) -> IO a
checkFileClosed (File fd closedRef) f = do
    closed <- readIORef closedRef
    if closed then throwECLOSED else f fd

-- | Set file's system offset.
--
-- Equivalent to <https://linux.die.net/man/3/lseek64 lseek64(3)>.
seek :: HasCallStack => File -> Int64 -> Whence -> IO Int64
seek uvf off w = checkFileClosed uvf $ \ fd -> throwUVIfMinus $ hs_seek fd off w

instance Input File where
    readInput f buf bufSiz = readFileP f buf bufSiz (-1)

-- | Read file with given offset
--
-- Read length may be smaller than buffer size.
readFileP :: HasCallStack
          => File
          -> Ptr Word8 -- ^ buffer
          -> Int       -- ^ buffer size
          -> Int64     -- ^ file offset, pass -1 to use default(system) offset
          -> IO Int    -- ^ read length
readFileP uvf buf bufSiz off =
    checkFileClosed uvf  $ \ fd -> do
        uvm <- getUVManager
        withUVRequest uvm (hs_uv_fs_read_threaded fd buf bufSiz off)

instance Output File where
    writeOutput f buf bufSiz = writeFileP f buf bufSiz (-1)

-- | Write buffer to file
--
-- This function will loop until all bytes are written.
--
-- Note on linux files opened with 'O_APPEND' behave differently since this function use @pwrite@:
--
-- @
-- POSIX requires that opening a file with the O_APPEND flag should have no effect
-- on the location at which pwrite() writes data. However, on Linux,
-- if a file is opened with O_APPEND, pwrite() appends data to the end of the file,
-- regardless of the value of offset.
-- @
writeFileP :: HasCallStack
           => File
           -> Ptr Word8 -- ^ buffer
           -> Int       -- ^ buffer size
           -> Int64     -- ^ file offset, pass -1 to use default(system) offset
           -> IO ()
writeFileP uvf buf0 bufSiz0 off0 =
    checkFileClosed uvf $ \ fd -> do
             (if off0 == -1 then go fd buf0 bufSiz0
                            else go' fd buf0 bufSiz0 off0)
  where
    -- use -1 offset to use fd's default offset
    go fd buf bufSiz = do
        uvm <- getUVManager
        written <- withUVRequest uvm
            (hs_uv_fs_write_threaded fd buf bufSiz (-1))
        when (written < bufSiz)
            (go fd (buf `plusPtr` written) (bufSiz-written))

    go' fd buf bufSiz !off = do
        uvm <- getUVManager
        written <- withUVRequest uvm
            (hs_uv_fs_write_threaded fd buf bufSiz off)
        when (written < bufSiz) $
            go' fd (buf `plusPtr` written)
                   (bufSiz-written)
                   (off+fromIntegral written)

-- | File bundled with offset.
--
-- Reading or writing using 'Input' \/ 'Output' instance will automatically increase offset.
-- 'FilePtrT' and its operations are NOT thread safe, use 'MVar' 'FilePtrT' in multiple threads.
--
-- The notes on linux 'writeFileP' applied to 'FilePtr' too.
data FilePtrT = FilePtrT {-# UNPACK #-} !File
                         {-# UNPACK #-} !(PrimIORef Int64)

-- |  Create a file offset bundle from an 'File'.
--
newFilePtrT :: File      -- ^ the file we're reading
            -> Int64      -- ^ initial offset
            -> IO FilePtrT
newFilePtrT uvf off = FilePtrT uvf <$> newPrimIORef off

-- | Get current offset.
getFilePtrOffset :: FilePtrT -> IO Int64
getFilePtrOffset (FilePtrT _ offsetRef) = readPrimIORef offsetRef

-- | Change current offset.
setFilePtrOffset :: FilePtrT -> Int64 -> IO ()
setFilePtrOffset (FilePtrT _ offsetRef) = writePrimIORef offsetRef

instance Input FilePtrT where
    readInput (FilePtrT file offsetRef) buf bufSiz =
        readPrimIORef offsetRef >>= \ off -> do
            l <- readFileP file buf bufSiz off
            writePrimIORef offsetRef (off + fromIntegral l)
            return l

instance Output FilePtrT where
    writeOutput (FilePtrT file offsetRef) buf bufSiz =
        readPrimIORef offsetRef >>= \ off -> do
            writeFileP file buf bufSiz off
            writePrimIORef offsetRef (off + fromIntegral bufSiz)

--------------------------------------------------------------------------------

-- | init a file 'Resource', which open a file when used.
--
-- Resource closing will wait for the referencing counter goes
-- down to zero (no reading or writing is in process), which can
-- be a problem if you are using multiple readers or writers in multiple threads.
-- In that case you have to stop all reading or writing thread if you don't want to
-- block the resource thread.
initFile :: CBytes
          -> FileFlag        -- ^ Opening flags, e.g. 'O_CREAT' @.|.@ 'O_RDWR'
          -> FileMode        -- ^ Sets the file mode (permission and sticky bits),
                               -- but only if the file was created, see 'DEFAULT_MODE'.
          -> Resource File
initFile path flags mode =
    initResource
        (do uvm <- getUVManager
            fd <- withCBytesUnsafe path $ \ p ->
                withUVRequest uvm (hs_uv_fs_open_threaded p flags mode)
            File (fromIntegral fd) <$> newIORef False)
        (\ (File fd closedRef) -> do
            closed <- readIORef closedRef
            unless closed $ do
                throwUVIfMinus_ (hs_uv_fs_close fd)
                writeIORef closedRef True)

-- | Quickly open a file and read its content.
readFile :: HasCallStack => CBytes -> IO V.Bytes
readFile filename = do
    withResource (initFile filename O_RDONLY DEFAULT_MODE) $ \ file -> do
        readAll' =<< newBufferedInput file

-- | Quickly open a file and read its content as UTF8 text.
readTextFile :: HasCallStack => CBytes -> IO T.Text
readTextFile filename = T.validate <$> readFile filename

-- | Quickly open a file and write some content.
writeFile :: HasCallStack => CBytes -> V.Bytes -> IO ()
writeFile filename content = do
    withResource (initFile filename (O_WRONLY .|. O_CREAT) DEFAULT_MODE) $ \ file -> do
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

-- | Equivalent to <http://linux.die.net/man/2/mkdir mkdir(2)>.
--
-- Note mode is currently not implemented on Windows. On unix you should set execute bit
-- if you want the directory is accessable, e.g. 0o777.
mkdir :: HasCallStack => CBytes -> FileMode -> IO ()
mkdir path mode = do
    uvm <- getUVManager
    withCBytesUnsafe path $ \ p ->
        withUVRequest_ uvm (hs_uv_fs_mkdir_threaded p mode)

-- | Equivalent to @mkdir -p@
--
-- Note mode is currently not implemented on Windows. On unix you should set execute bit
-- if you want the directory is accessable(so that child folder can be created), e.g. 0o777.
mkdirp :: HasCallStack => CBytes -> FileMode -> IO ()
mkdirp path mode = do
    uvm <- getUVManager
    r <- withCBytesUnsafe path $ \ p ->
        withUVRequest' uvm (hs_uv_fs_mkdir_threaded p mode)
            (\ r -> do
                when (r < 0 && fromIntegral r /= UV_ENOENT)
                    (throwUVIfMinus_ (return r))
                return r)
    when (fromIntegral r == UV_ENOENT) $ do
        (root, segs) <- P.splitSegments path
        case segs of
            seg:segs' -> loop segs' =<< P.join root seg
            _ -> throwUVIfMinus_ (return r)
  where
    loop segs p = do
        a <- access p F_OK
        case a of
            AccessOK     -> return ()
            NoExistence  -> mkdir p mode
            NoPermission -> throwUVIfMinus_ (return UV_EACCES)
        case segs of
            (nextp:ps) -> P.join p nextp >>= loop ps
            _  -> return ()

-- | Equivalent to <http://linux.die.net/man/2/unlink unlink(2)>.
unlink :: HasCallStack => CBytes -> IO ()
unlink path = do
    uvm <- getUVManager
    withCBytesUnsafe path $ \ p ->
        withUVRequest_ uvm (hs_uv_fs_unlink_threaded p)

-- | Equivalent to <mkdtemp http://linux.die.net/man/3/mkdtemp>
--
-- Creates a temporary directory in the most secure manner possible.
-- There are no race conditions in the directory’s creation.
-- The directory is readable, writable, and searchable only by the creating user ID.
-- The user of mkdtemp() is responsible for deleting the temporary directory and
-- its contents when done with it.
--
-- Note: the argument is the prefix of the temporary directory,
-- so no need to add XXXXXX ending.
mkdtemp :: HasCallStack => CBytes -> IO CBytes
mkdtemp path = do
    let size = CBytes.length path
    withCBytesUnsafe path $ \ p -> do
        (p'', _) <- CBytes.allocCBytesUnsafe (size+7) $ \ p' -> do  -- we append "XXXXXX\NUL" in C
            uvm <- getUVManager
            withUVRequest_ uvm (hs_uv_fs_mkdtemp_threaded p size p')
        return p''

-- | The same as 'Z.IO.FileSystem.Base.mkstemp', but a threaded version.
mkstemp :: HasCallStack => CBytes -> IO CBytes
mkstemp template = do
    let size = CBytes.length template
    CBytes.withCBytesUnsafe template $ \ p -> do
        (p'', _) <- CBytes.allocCBytesUnsafe (size+7) $ \ p' -> do  -- we append "XXXXXX\NUL" in C
            uvm <- getUVManager
            withUVRequest_ uvm (hs_uv_fs_mkstemp_threaded p size p')
        return p''

-- | Equivalent to <http://linux.die.net/man/2/rmdir rmdir(2)>.
--
-- Note this function may inherent OS limitations such as argument must be an empty folder.
rmdir :: HasCallStack => CBytes -> IO ()
rmdir path = do
    uvm <- getUVManager
    withCBytesUnsafe path (\ p -> void . withUVRequest uvm $ hs_uv_fs_rmdir_threaded p)

-- | Equivalent to @rmdir -rf@
--
-- This function will try to remove folder and files contained by it.
rmdirrf :: HasCallStack => CBytes -> IO ()
rmdirrf path = do
    ds <- scandir path
    forM_ ds $ \ (d, t) -> do
        if t /= DirEntDir
        then unlink d
        else rmdirrf =<< path `P.join` d
    rmdir path

--------------------------------------------------------------------------------

-- | Equivalent to <http://linux.die.net/man/3/scandir scandir(3)>.
--
-- Note Unlike scandir(3), this function does not return the “.” and “..” entries.
--
-- Note On Linux, getting the type of an entry is only supported by some file systems (btrfs, ext2, ext3 and ext4 at the time of this writing), check the <http://linux.die.net/man/2/getdents getdents(2)> man page.
scandir :: HasCallStack => CBytes -> IO [(CBytes, DirEntType)]
scandir path = do
    uvm <- getUVManager
    bracket
        (withCBytesUnsafe path $ \ p ->
            allocPrimSafe $ \ dents ->
                withUVRequestEx uvm
                    (hs_uv_fs_scandir_threaded p dents)
                    (hs_uv_fs_scandir_extra_cleanup dents))
        (\ (dents, n) -> hs_uv_fs_scandir_cleanup dents n)
        (\ (dents, n) -> forM [0..n-1] $ \ i -> do
            dent <- peekElemOff dents i
            (p, typ) <- peekUVDirEnt dent
            let !typ' = fromUVDirEntType typ
            !p' <- fromCString p
            return (p', typ'))

-- | Find all files and directories within a given directory with a predicator.
--
-- @
--  import Z.IO.FileSystem.FilePath (splitExtension)
--  -- find all haskell source file within current dir
--  scandirRecursively "."  (\ p _ -> (== ".hs") . snd <$> splitExtension p)
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

-- | Equivalent to <http://linux.die.net/man/2/stat stat(2)>
stat :: HasCallStack => CBytes -> IO FStat
stat path = do
    withCBytesUnsafe path $ \ p ->
         allocaBytes uvStatSize $ \ s -> do
            uvm <- getUVManager
            withUVRequest_ uvm (hs_uv_fs_stat_threaded p s)
            peekUVStat s

-- | Equivalent to <http://linux.die.net/man/2/lstat lstat(2)>
lstat :: HasCallStack => CBytes -> IO FStat
lstat path =
    withCBytesUnsafe path $ \ p ->
         allocaBytes uvStatSize $ \ s -> do
            uvm <- getUVManager
            withUVRequest_ uvm (hs_uv_fs_lstat_threaded p s)
            peekUVStat s

-- | Equivalent to <http://linux.die.net/man/2/fstat fstat(2)>
fstat :: HasCallStack => File -> IO FStat
fstat uvf = checkFileClosed uvf $ \ fd ->
     (allocaBytes uvStatSize $ \ s -> do
        uvm <- getUVManager
        withUVRequest_ uvm (hs_uv_fs_fstat_threaded fd s)
        peekUVStat s)

-- | If given path is a symbolic link?
isLink :: HasCallStack => CBytes -> IO Bool
isLink p = lstat p >>= \ st -> return (stMode st .&. S_IFMT == S_IFLNK)

-- | If given path is a directory or a symbolic link to a directory?
isDir :: HasCallStack => CBytes -> IO Bool
isDir p = stat p >>= \ st -> return (stMode st .&. S_IFMT == S_IFDIR)

-- | If given path is a file or a symbolic link to a file?
isFile :: HasCallStack => CBytes -> IO Bool
isFile p = stat p >>= \ st -> return (stMode st .&. S_IFMT == S_IFREG)

--------------------------------------------------------------------------------

-- | Equivalent to <http://linux.die.net/man/2/rename rename(2)>.
--
-- Note On Windows if this function fails with UV_EBUSY, UV_EPERM or UV_EACCES, it will retry to rename the file up to four times with 250ms wait between attempts before giving up. If both path and new_path are existing directories this function will work only if target directory is empty.
rename :: HasCallStack => CBytes -> CBytes -> IO ()
rename path path' = do
    uvm <- getUVManager
    withCBytesUnsafe path $ \ p ->
        withCBytesUnsafe path' $ \ p' ->
            withUVRequest_ uvm (hs_uv_fs_rename_threaded p p')

-- | Equivalent to <http://linux.die.net/man/2/fsync fsync(2)>.
fsync :: HasCallStack => File -> IO ()
fsync uvf = checkFileClosed uvf $ \ fd -> do
    uvm <- getUVManager
    withUVRequest_ uvm (hs_uv_fs_fsync_threaded fd)

-- | Equivalent to <http://linux.die.net/man/2/fdatasync fdatasync(2)>.
fdatasync :: HasCallStack => File -> IO ()
fdatasync uvf = checkFileClosed uvf $ \ fd -> do
    uvm <- getUVManager
    withUVRequest_ uvm (hs_uv_fs_fdatasync_threaded fd)

-- | Equivalent to <http://linux.die.net/man/2/ftruncate ftruncate(2)>.
ftruncate :: HasCallStack => File -> Int64 -> IO ()
ftruncate uvf off = checkFileClosed uvf $ \ fd -> do
    uvm <- getUVManager
    withUVRequest_ uvm (hs_uv_fs_ftruncate_threaded fd off)

-- | Copies a file from path to new_path.
--
-- Warning: If the destination path is created, but an error occurs while copying the data, then the destination path is removed. There is a brief window of time between closing and removing the file where another process could access the file.
copyfile :: HasCallStack => CBytes -> CBytes -> CopyFileFlag -> IO ()
copyfile path path' flag = do
    uvm <- getUVManager
    withCBytesUnsafe path $ \ p ->
        withCBytesUnsafe path' $ \ p' ->
            withUVRequest_ uvm (hs_uv_fs_copyfile_threaded p p' flag)

-- | Equivalent to <http://linux.die.net/man/2/access access(2)> on Unix.
-- Windows uses GetFileAttributesW().
access :: HasCallStack => CBytes -> AccessMode -> IO AccessResult
access path mode = do
    uvm <- getUVManager
    withCBytesUnsafe path $ \ p ->
        withUVRequest' uvm (hs_uv_fs_access_threaded p mode) (handleResult . fromIntegral)
  where
    handleResult r
        | r == 0           = return AccessOK
        | r == UV_ENOENT   = return NoExistence
        | r == UV_EACCES   = return NoPermission
        | otherwise        = do
            name <- uvErrName r
            desc <- uvStdError r
            throwUVError r (IOEInfo name desc callStack)

-- | Equivalent to <http://linux.die.net/man/2/chmod chmod(2)>.
chmod :: HasCallStack => CBytes -> FileMode -> IO ()
chmod path mode = do
    uvm <- getUVManager
    withCBytesUnsafe path $ \ p ->
        withUVRequest_ uvm (hs_uv_fs_chmod_threaded p mode)

-- | Equivalent to <http://linux.die.net/man/2/fchmod fchmod(2)>.
fchmod :: HasCallStack => File -> FileMode -> IO ()
fchmod uvf mode = checkFileClosed uvf $ \ fd -> do
    uvm <- getUVManager
    withUVRequest_ uvm (hs_uv_fs_fchmod_threaded fd mode)

-- | Equivalent to <http://linux.die.net/man/2/utime utime(2)>.
--
-- libuv choose 'Double' type due to cross platform concerns, we only provide micro-second precision.
--
utime :: HasCallStack
      => CBytes
      -> Double     -- ^ atime, i.e. access time
      -> Double     -- ^ mtime, i.e. modify time
      -> IO ()
utime path atime mtime = do
    uvm <- getUVManager
    withCBytesUnsafe path $ \ p ->
        withUVRequest_ uvm (hs_uv_fs_utime_threaded p atime mtime)

-- | Equivalent to <https://man7.org/linux/man-pages/man3/futimes.3.html futime(3)>.
--
-- Same precision notes with 'utime'.
futime :: HasCallStack => File -> Double -> Double -> IO ()
futime uvf atime mtime = checkFileClosed uvf $ \ fd -> do
    uvm <- getUVManager
    withUVRequest_ uvm (hs_uv_fs_futime_threaded fd atime mtime)

-- | Equivalent to <https://man7.org/linux/man-pages/man3/lutimes.3.html lutime(3)>.
--
-- Same precision notes with 'utime'.
lutime :: HasCallStack
       => CBytes
       -> Double     -- ^ atime, i.e. access time
       -> Double     -- ^ mtime, i.e. modify time
       -> IO ()
lutime path atime mtime = do
    uvm <- getUVManager
    withCBytesUnsafe path $ \ p ->
        withUVRequest_ uvm (hs_uv_fs_lutime_threaded p atime mtime)

-- | Equivalent to <http://linux.die.net/man/2/link link(2)>.
link :: HasCallStack => CBytes -> CBytes -> IO ()
link path path' = do
    uvm <- getUVManager
    withCBytesUnsafe path $ \ p ->
        withCBytesUnsafe path' $ \ p' ->
            withUVRequest_ uvm (hs_uv_fs_link_threaded p p')

-- | Equivalent to <http://linux.die.net/man/2/symlink symlink(2)>.
--
-- | Note On Windows the flags parameter can be specified to control how the symlink will be created.
--
--   * 'SYMLINK_DIR': indicates that path points to a directory.
--   * 'SYMLINK_JUNCTION': request that the symlink is created using junction points.
--
-- On other platforms these flags are ignored.
symlink :: HasCallStack => CBytes -> CBytes -> SymlinkFlag -> IO ()
symlink path path' flag = do
    uvm <- getUVManager
    withCBytesUnsafe path $ \ p ->
        withCBytesUnsafe path' $ \ p' ->
            withUVRequest_ uvm (hs_uv_fs_symlink_threaded p p' flag)

-- | Equivalent to <http://linux.die.net/man/2/readlink readlink(2)>.
readlink :: HasCallStack => CBytes -> IO CBytes
readlink path = do
    uvm <- getUVManager
    bracket
        (withCBytesUnsafe path $ \ p ->
            allocPrimSafe $ \ p' ->
                withUVRequestEx uvm
                    (hs_uv_fs_readlink_threaded p p')
                    (\ _ -> hs_uv_fs_readlink_extra_cleanup p'))
        (hs_uv_fs_readlink_cleanup . fst)
        (fromCString . fst)

-- | Equivalent to <http://linux.die.net/man/3/realpath realpath(3)> on Unix. Windows uses <https://msdn.microsoft.com/en-us/library/windows/desktop/aa364962(v=vs.85).aspx GetFinalPathNameByHandle>.
--
-- Warning This function has certain platform-specific caveats that were discovered when used in Node.
--
--  * macOS and other BSDs: this function will fail with UV_ELOOP if more than 32 symlinks are found while
--    resolving the given path. This limit is hardcoded and cannot be sidestepped.
--
--  * Windows: while this function works in the common case, there are a number of corner cases where it doesn’t:
--
--      * Paths in ramdisk volumes created by tools which sidestep the Volume Manager (such as ImDisk) cannot be resolved.
--      * Inconsistent casing when using drive letters.
--      * Resolved path bypasses subst’d drives.
--
-- While this function can still be used, it’s not recommended if scenarios such as the above need to be supported.
-- The background story and some more details on these issues can be checked <https://github.com/nodejs/node/issues/7726 here>.
--
-- Note This function is not implemented on Windows XP and Windows Server 2003. On these systems, UV_ENOSYS is returned.
realpath :: HasCallStack => CBytes -> IO CBytes
realpath path = do
    uvm <- getUVManager
    bracket
        (withCBytesUnsafe path $ \ p ->
            allocPrimSafe $ \ p' ->
                withUVRequestEx uvm
                    (hs_uv_fs_realpath_threaded p p')
                    (\ _ -> hs_uv_fs_readlink_extra_cleanup p'))
        (hs_uv_fs_readlink_cleanup . fst)
        (fromCString . fst)

-- | Equivalent to <http://linux.die.net/man/2/chown chown(2)>.
chown :: HasCallStack => CBytes -> UID -> GID -> IO ()
chown path uid gid = do
    uvm <- getUVManager
    withCBytesUnsafe path $ \ p ->
        withUVRequest_ uvm (hs_uv_fs_chown_threaded p uid gid)

-- | Equivalent to <http://linux.die.net/man/2/fchown fchown(2)>.
fchown :: HasCallStack => FD -> UID -> GID -> IO ()
fchown fd uid gid = do
    uvm <- getUVManager
    withUVRequest_ uvm (hs_uv_fs_fchown_threaded fd uid gid)

-- | Equivalent to <http://linux.die.net/man/2/lchown lchown(2)>.
lchown :: HasCallStack => CBytes -> UID -> GID -> IO ()
lchown path uid gid = do
    uvm <- getUVManager
    withCBytesUnsafe path $ \ p ->
        withUVRequest_ uvm (hs_uv_fs_lchown_threaded p uid gid)
