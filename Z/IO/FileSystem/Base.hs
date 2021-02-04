{-|
Module      : Z.IO.FileSystem.Base
Description : Filesystem IO
Copyright   : (c) Dong Han, 2017-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide IO operations related to filesystem, operations are
implemented using unsafe FFIs, which should be prefered when the operations'
estimated time is short(<1ms), which is much common on modern SSDs.
-}
module Z.IO.FileSystem.Base
  ( -- * Regular file devices
    File, initFile, readFileP, writeFileP, getFileFD, seek
  , readFile, readTextFile, writeFile, writeTextFile
  , readJSONFile, writeJSONFile
    -- * file offset bundle
  , FilePtr, newFilePtr, getFilePtrOffset, setFilePtrOffset
  -- * Filesystem operations
  , mkdir, mkdirp
  , unlink
  , mkdtemp, mkstemp , initTempFile, initTempDir
  , rmdir, rmrf
  , DirEntType(..)
  , scandir
  , scandirRecursively
    -- ** File stats
  , FStat(..), UVTimeSpec(..)
  , doesPathExist, doesFileExist, doesDirExist
  , isLink, isDir, isFile
  , isLinkSt, isDirSt, isFileSt
  , stat, lstat, fstat
  , stat', lstat'
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
  , pattern DEFAULT_FILE_MODE
  , pattern DEFAULT_DIR_MODE
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
  -- ** Whence
  , Whence
  , pattern SEEK_SET
  , pattern SEEK_CUR
  , pattern SEEK_END
  ) where

import           Control.Monad
import           Data.Bits
import           Data.IORef
import           Data.Int
import           Data.Word
import           Foreign.Marshal.Alloc    (allocaBytes)
import           Foreign.Ptr
import           Foreign.Storable         (peekElemOff)
import           Prelude                  hiding (readFile, writeFile)
import qualified Z.Data.Builder           as B
import           Z.Data.CBytes            as CBytes
import qualified Z.Data.JSON              as JSON
import           Z.Data.PrimRef.PrimIORef
import qualified Z.Data.Text              as T
import qualified Z.Data.Text.Print        as T
import qualified Z.Data.Vector            as V
import           Z.Foreign
import           Z.IO.Buffered
import qualified Z.IO.Environment         as Env
import           Z.IO.Exception
import qualified Z.IO.FileSystem.FilePath as P
import           Z.IO.Resource
import           Z.IO.UV.FFI

#include "_Shared.hs"

--------------------------------------------------------------------------------
-- File

-- | 'File' and its operations are NOT thread safe, use 'MVar' 'File' in multiple threads
--
-- libuv implements read and write method with both implict and explict offset capable.
-- Implict offset interface is provided by 'Input' \/ 'Output' instances.
-- Explict offset interface is provided by 'readFileP' \/ 'writeFileP'.
--
data File =  File  {-# UNPACK #-} !FD      -- ^ the file
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
    -- readInput :: HasCallStack => File -> Ptr Word8 -> Int -> IO Int
    -- use -1 offset to use fd's default offset
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
    checkFileClosed uvf $ \ fd -> throwUVIfMinus $ hs_uv_fs_read fd buf bufSiz off

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
    checkFileClosed uvf $ \fd ->  if off0 == -1 then go fd buf0 bufSiz0
                                                else go' fd buf0 bufSiz0 off0
  where
    go fd !buf !bufSiz = do
        written <- throwUVIfMinus (hs_uv_fs_write fd buf bufSiz (-1))
        when (written < bufSiz)
            (go fd (buf `plusPtr` written) (bufSiz-written))

    go' fd !buf !bufSiz !off = do
        written <- throwUVIfMinus (hs_uv_fs_write fd buf bufSiz off)
        when (written < bufSiz) $
            go' fd (buf `plusPtr` written)
                   (bufSiz-written)
                   (off+fromIntegral written)


--------------------------------------------------------------------------------

-- | init a file 'Resource', which open a file when used.
--
-- Resource closing is thread safe, on some versions of OSX, repeatly open and close same file 'Resource' may
-- result in shared memory object error, use 'O_CREAT' to avoid that.
initFile :: HasCallStack
         => CBytes
         -> FileFlag        -- ^ Opening flags, e.g. 'O_CREAT' @.|.@ 'O_RDWR'
         -> FileMode      -- ^ Sets the file mode (permission and sticky bits),
                            -- but only if the file was created, see 'DEFAULT_FILE_MODE'.
         -> Resource File
initFile path flags mode =
    initResource
        (do !fd <- withCBytesUnsafe path $ \ p ->
                throwUVIfMinus $ hs_uv_fs_open p flags mode
            File fd <$> newIORef False)
        (\ (File fd closedRef) -> do
            closed <- readIORef closedRef
            unless closed $ do
                throwUVIfMinus_ (hs_uv_fs_close fd)
                writeIORef closedRef True)

--------------------------------------------------------------------------------

-- | Create a directory named path with numeric mode 'FileMode'.
--
-- Equivalent to <http://linux.die.net/man/2/mkdir mkdir(2)>.
--
-- Note mode is currently not implemented on Windows. On unix you should set execute bit
-- if you want the directory is accessable, e.g. 0o777.
mkdir :: HasCallStack => CBytes -> FileMode -> IO ()
mkdir path mode = throwUVIfMinus_ . withCBytesUnsafe path $ \ p ->
     hs_uv_fs_mkdir p mode

-- | Recursive directory creation function. Like 'mkdir', but makes all
-- intermediate-level directories needed to contain the leaf directory.
--
-- Equivalent to @mkdir -p@,
--
-- Note mode is currently not implemented on Windows. On unix you should set
-- execute bit if you want the directory is accessable(so that child folder
-- can be created), e.g. 0o777.
--
-- >>> mkdirp "p/a" DEFAULT_DIR_MODE
mkdirp :: HasCallStack => CBytes -> FileMode -> IO ()
mkdirp path mode = do
    r <- withCBytesUnsafe path $ \ p -> hs_uv_fs_mkdir p mode
    case fromIntegral r of
        UV_ENOENT -> do
            (root, segs) <- P.splitSegments path
            case segs of
                seg:segs' -> loop segs' =<< P.join root seg
                _         -> throwUV r
        UV_EEXIST -> do
            canIgnore <- isDir path
            unless canIgnore $ throwUV r
        _ -> throwUV r
  where
    loop segs p = do
        a <- access p F_OK
        case a of
            AccessOK     -> return ()
            NoExistence  -> mkdir p mode
            NoPermission -> throwUV UV_EACCES
        case segs of
            (nextp:ps) -> P.join p nextp >>= loop ps
            _          -> return ()

-- | Equivalent to <http://linux.die.net/man/2/unlink unlink(2)>.
unlink :: HasCallStack => CBytes -> IO ()
unlink path = throwUVIfMinus_ (withCBytesUnsafe path hs_uv_fs_unlink)


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
--
mkdtemp :: HasCallStack => CBytes -> IO CBytes
mkdtemp path = do
    let size = CBytes.length path
    withCBytesUnsafe path $ \ p -> do
        (p',_) <- CBytes.allocCBytesUnsafe (size+7) $ \ p' -> do  -- we append "XXXXXX\NUL" in C
            throwUVIfMinus_ (hs_uv_fs_mkdtemp p size p')
        return p'

-- | Equivalent to <mkstemp https://man7.org/linux/man-pages/man3/mkstemp.3.html>
mkstemp :: HasCallStack => CBytes -> IO CBytes
mkstemp template = do
    let size = CBytes.length template
    CBytes.withCBytesUnsafe template $ \p -> do
        (p', _) <- CBytes.allocCBytesUnsafe (size + 7) $ \ p' -> do  -- we append "XXXXXX\NUL" in C
            throwUVIfMinus_ (hs_uv_fs_mkstemp p size p')
        return p'

-------------------------------------------------------------------------------

-- | Equivalent to <http://linux.die.net/man/2/rmdir rmdir(2)>.
--
-- Note this function may inherent OS limitations such as argument must be an empty folder.
rmdir :: HasCallStack => CBytes -> IO ()
rmdir path = throwUVIfMinus_ (withCBytesUnsafe path hs_uv_fs_rmdir)

-- | Removes a file or directory at path together with its contents and
-- subdirectories. Symbolic links are removed without affecting their targets.
-- If the path does not exist, nothing happens.
rmrf :: HasCallStack => CBytes -> IO ()
rmrf path =
    withCBytesUnsafe path $ \path' ->
    allocaBytes uvStatSize $ \s -> do
        r <- fromIntegral <$> hs_uv_fs_stat path' s
        if  | r == UV_ENOENT -> pure ()   -- nothing if path does not exist.
            | r < 0     -> throwUV r
            | otherwise -> do
                st <- peekUVStat s
                case stMode st .&. S_IFMT of
                    S_IFREG -> unlink path
                    S_IFLNK -> unlink path
                    S_IFDIR -> do
                        ds <- scandir path
                        forM_ ds $ \ (d, t) ->
                            if t /= DirEntDir
                            then unlink d
                            else rmrf =<< path `P.join` d
                        rmdir path
                    mode    -> do
                        let desc = B.buildText $ "Unsupported file mode: " >> B.hex mode
                        throwIO $ UnsupportedOperation (IOEInfo "" desc callStack)

-- | Equivalent to <http://linux.die.net/man/3/scandir scandir(3)>.
--
-- Note Unlike scandir(3), this function does not return the “.” and “..” entries.
--
-- Note On Linux, getting the type of an entry is only supported by some file
-- systems (btrfs, ext2, ext3 and ext4 at the time of this writing), check the
-- <http://linux.die.net/man/2/getdents getdents(2)> man page.
scandir :: HasCallStack => CBytes -> IO [(CBytes, DirEntType)]
scandir path = do
    bracket
        (withCBytesUnsafe path $ \ p ->
            allocPrimUnsafe $ \ dents ->
                throwUVIfMinus (hs_uv_fs_scandir p dents))
        (\ (dents, n) -> hs_uv_fs_scandir_cleanup dents n)
        (\ (dents, n) -> forM [0..n-1] $ \ i -> do
            dent <- peekElemOff dents i
            (p, typ) <- peekUVDirEnt dent
            let !typ' = fromUVDirEntType typ
            !p' <- fromCString p
            return (p', typ'))

--------------------------------------------------------------------------------
-- File Status

-- | Equivalent to <http://linux.die.net/man/2/stat stat(2)>
stat :: HasCallStack => CBytes -> IO FStat
stat path = withCBytesUnsafe path $ \ p ->
     allocaBytes uvStatSize $ \ s -> do
        throwUVIfMinus_ (hs_uv_fs_stat p s)
        peekUVStat s

-- | Equivalent to <http://linux.die.net/man/2/lstat lstat(2)>
lstat :: HasCallStack => CBytes -> IO FStat
lstat path = withCBytesUnsafe path $ \ p ->
     allocaBytes uvStatSize $ \ s -> do
        throwUVIfMinus_ (hs_uv_fs_lstat p s)
        peekUVStat s

-- | Equivalent to <http://linux.die.net/man/2/stat stat(2)>
--
-- Return 'Nothing' instead of throwing 'NoSuchThing' if the file doesn't exist.
stat' :: HasCallStack => CBytes -> IO (Maybe FStat)
stat' path = withCBytesUnsafe path $ \ p ->
     allocaBytes uvStatSize $ \ s -> do
        r <- fromIntegral <$> hs_uv_fs_stat p s
        if  | r == UV_ENOENT -> return Nothing
            | r < 0 -> throwUV r
            | otherwise -> Just <$> peekUVStat s

-- | Equivalent to <http://linux.die.net/man/2/lstat lstat(2)>
--
-- Return 'Nothing' instead of throwing 'NoSuchThing' if the link doesn't exist.
lstat' :: HasCallStack => CBytes -> IO (Maybe FStat)
lstat' path = withCBytesUnsafe path $ \ p ->
     allocaBytes uvStatSize $ \ s -> do
        r <- fromIntegral <$> hs_uv_fs_lstat p s
        if  | r == UV_ENOENT -> return Nothing
            | r < 0 -> throwUV r
            | otherwise -> Just <$> peekUVStat s

-- | Equivalent to <http://linux.die.net/man/2/fstat fstat(2)>
fstat :: HasCallStack => File -> IO FStat
fstat uvf = checkFileClosed uvf $ \ fd ->
    allocaBytes uvStatSize $ \ s -> do
        throwUVIfMinus_ (hs_uv_fs_fstat fd s)
        peekUVStat s

--------------------------------------------------------------------------------

-- | Equivalent to <http://linux.die.net/man/2/rename rename(2)>.
--
-- Note On Windows if this function fails with UV_EBUSY, UV_EPERM or UV_EACCES, it will retry to rename the file up to four times with 250ms wait between attempts before giving up. If both path and new_path are existing directories this function will work only if target directory is empty.
rename :: HasCallStack => CBytes -> CBytes -> IO ()
rename path path' = throwUVIfMinus_ . withCBytesUnsafe path $ \ p ->
    withCBytesUnsafe path' (hs_uv_fs_rename p)

-- | Equivalent to <http://linux.die.net/man/2/fsync fsync(2)>.
fsync :: HasCallStack => File -> IO ()
fsync uvf = checkFileClosed uvf $ \ fd -> throwUVIfMinus_ $ hs_uv_fs_fsync fd

-- | Equivalent to <http://linux.die.net/man/2/fdatasync fdatasync(2)>.
fdatasync :: HasCallStack => File -> IO ()
fdatasync uvf = checkFileClosed uvf $ \ fd -> throwUVIfMinus_ $ hs_uv_fs_fdatasync fd

-- | Equivalent to <http://linux.die.net/man/2/ftruncate ftruncate(2)>.
ftruncate :: HasCallStack => File -> Int64 -> IO ()
ftruncate uvf off = checkFileClosed uvf $ \ fd -> throwUVIfMinus_ $ hs_uv_fs_ftruncate fd off

-- | Copies a file from path to new_path.
--
-- Warning: If the destination path is created, but an error occurs while copying the data, then the destination path is removed. There is a brief window of time between closing and removing the file where another process could access the file.
copyfile :: HasCallStack => CBytes -> CBytes -> CopyFileFlag -> IO ()
copyfile path path' flag = throwUVIfMinus_ . withCBytesUnsafe path $ \ p ->
    withCBytesUnsafe path' $ \ p' -> hs_uv_fs_copyfile p p' flag

-- | Equivalent to <http://linux.die.net/man/2/access access(2)> on Unix.
--
-- Windows uses GetFileAttributesW().
access :: HasCallStack => CBytes -> AccessMode -> IO AccessResult
access path mode = do
     r <- withCBytesUnsafe path $ \ p -> fromIntegral <$> hs_uv_fs_access p mode
     if | r == 0           -> return AccessOK
        | r == UV_ENOENT   -> return NoExistence
        | r == UV_EACCES   -> return NoPermission
        | otherwise        -> do
            name <- uvErrName r
            desc <- uvStdError r
            throwUVError r (IOEInfo name desc callStack)

-- | Equivalent to <http://linux.die.net/man/2/chmod chmod(2)>.
chmod :: HasCallStack => CBytes -> FileMode -> IO ()
chmod path mode = throwUVIfMinus_ . withCBytesUnsafe path $ \ p -> hs_uv_fs_chmod p mode

-- | Equivalent to <http://linux.die.net/man/2/fchmod fchmod(2)>.
fchmod :: HasCallStack => File -> FileMode -> IO ()
fchmod uvf mode = checkFileClosed uvf $ \ fd -> throwUVIfMinus_ $ hs_uv_fs_fchmod fd mode

-- | Equivalent to <http://linux.die.net/man/2/utime utime(2)>.
--
-- libuv choose 'Double' type due to cross platform concerns, we only provide micro-second precision.
utime :: HasCallStack
      => CBytes
      -> Double     -- ^ atime, i.e. access time
      -> Double     -- ^ mtime, i.e. modify time
      -> IO ()
utime path atime mtime = throwUVIfMinus_ . withCBytesUnsafe path $ \ p -> hs_uv_fs_utime p atime mtime

-- | Equivalent to <https://man7.org/linux/man-pages/man3/futimes.3.html futime(3)>.
--
-- Same precision notes with 'utime'.
futime :: HasCallStack => File -> Double -> Double -> IO ()
futime uvf atime mtime = checkFileClosed uvf $ \ fd ->
    throwUVIfMinus_ (hs_uv_fs_futime fd atime mtime)

-- | Equivalent to <https://man7.org/linux/man-pages/man3/lutimes.3.html lutime(3)>.
--
-- Same precision notes with 'utime'.
lutime :: HasCallStack
       => CBytes
       -> Double     -- ^ atime, i.e. access time
       -> Double     -- ^ mtime, i.e. modify time
       -> IO ()
lutime path atime mtime = throwUVIfMinus_ . withCBytesUnsafe path $ \ p -> hs_uv_fs_lutime p atime mtime

-- | Equivalent to <http://linux.die.net/man/2/link link(2)>.
link :: HasCallStack => CBytes -> CBytes -> IO ()
link path path' = throwUVIfMinus_ . withCBytesUnsafe path $ \ p ->
    withCBytesUnsafe path' $ hs_uv_fs_link p

-- | Equivalent to <http://linux.die.net/man/2/symlink symlink(2)>.
--
-- | Note On Windows the flags parameter can be specified to control how the symlink will be created.
--
--   * 'SYMLINK_DIR': indicates that path points to a directory.
--   * 'SYMLINK_JUNCTION': request that the symlink is created using junction points.
--
-- On other platforms these flags are ignored.
symlink :: HasCallStack => CBytes -> CBytes -> SymlinkFlag -> IO ()
symlink path path' flag = throwUVIfMinus_ . withCBytesUnsafe path $ \ p ->
    withCBytesUnsafe path' $ \ p' -> hs_uv_fs_symlink p p' flag

-- | Equivalent to <http://linux.die.net/man/2/readlink readlink(2)>.
readlink :: HasCallStack => CBytes -> IO CBytes
readlink path = do
    bracket
        (withCBytesUnsafe path $ \ p ->
            allocPrimUnsafe $ \ p' ->
                throwUVIfMinus (hs_uv_fs_readlink p p'))
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
    bracket
        (withCBytesUnsafe path $ \ p ->
            allocPrimUnsafe $ \ p' ->
                throwUVIfMinus (hs_uv_fs_realpath p p'))
        (hs_uv_fs_readlink_cleanup . fst)
        (fromCString . fst)

-- | Equivalent to <http://linux.die.net/man/2/chown chown(2)>.
chown :: HasCallStack => CBytes -> UID -> GID -> IO ()
chown path uid gid = throwUVIfMinus_ . withCBytesUnsafe path $ \ p -> hs_uv_fs_chown p uid gid

-- | Equivalent to <http://linux.die.net/man/2/fchown fchown(2)>.
fchown :: HasCallStack => File -> UID -> GID -> IO ()
fchown uvf uid gid = checkFileClosed uvf $ \ fd -> throwUVIfMinus_ $ hs_uv_fs_fchown fd uid gid

-- | Equivalent to <http://linux.die.net/man/2/lchown lchown(2)>.
lchown :: HasCallStack => CBytes -> UID -> GID -> IO ()
lchown path uid gid = throwUVIfMinus_ . withCBytesUnsafe path $ \ p -> hs_uv_fs_lchown p uid gid
