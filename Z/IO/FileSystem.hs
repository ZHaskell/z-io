{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Z.IO.FileSystem
Description : Filesystem IO
Copyright   : (c) Dong Han, 2017~2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide IO operations related to filesystem, operations are implemented using unsafe FFIs, which should be prefered when the operations' estimated time is short(<1ms), which is much common on modern SSDs.

-}

module Z.IO.FileSystem
  ( -- * regular file devices
    File, initFile, readFile, writeFile
    -- * opening constant
  , FileMode(DEFAULT_MODE, S_IRWXU, S_IRUSR, S_IWUSR
      , S_IXUSR, S_IRWXG, S_IRGRP, S_IWGRP, S_IXGRP, S_IRWXO, S_IROTH
      )
  , FileFlag(O_APPEND, O_CREAT, O_DIRECT, O_DSYNC, O_EXCL
      , O_EXLOCK, O_NOATIME, O_NOFOLLOW, O_RDONLY, O_RDWR, O_SYMLINK
      , O_SYNC, O_TRUNC, O_WRONLY, O_RANDOM, O_SHORT_LIVED, O_SEQUENTIAL, O_TEMPORARY
      )
  -- * filesystem operations
  , mkdir
  , unlink
  , mkdtemp
  , rmdir
  , DirEntType(..)
  , scandir
  , UVStat(..), UVTimeSpec(..)
  , stat, lstat, fstat
  , rename
  , fsync, fdatasync
  , ftruncate
  , UVCopyFileFlag(COPYFILE_DEFAULT, COPYFILE_EXCL, COPYFILE_FICLONE)
  , copyfile
  , UVAccessMode(F_OK, R_OK, W_OK, X_OK)
  , AccessResult(..)
  , access
  , chmod, fchmod
  , utime, futime
  , UVSymlinkFlag(SYMLINK_DEFAULT, SYMLINK_DIR, SYMLINK_JUNCTION)
  , link, symlink
  , readlink, realpath
  ) where

import           Control.Monad
import           Data.Word
import           Data.Int
import           Data.IORef
import           Foreign.Ptr
import           Foreign.Storable               (peekElemOff)
import           Foreign.Marshal.Alloc          (allocaBytes)
import           Z.Data.CBytes                 as CBytes
import           Z.Foreign
import           Z.IO.Buffered
import           Z.IO.Exception
import           Z.IO.Resource
import           Z.IO.UV.Errno
import           Z.IO.UV.FFI
import           Prelude hiding (writeFile, readFile)

--------------------------------------------------------------------------------
-- File

-- | 'File' and its operations are NOT thread safe, use 'MVar' 'File' in multiple threads
--
-- libuv implements read and write method with both implict and explict offset capable.
-- Implict offset interface is provided by 'Input' \/ 'Output' instances.
-- Explict offset interface is provided by 'readFile' \/ 'writeFile'.
--
data File =  File  {-# UNPACK #-} !UVFD      -- ^ the file
                   {-# UNPACK #-} !(IORef Bool)  -- ^ closed flag

-- | If fd is -1 (closed), throw 'ResourceVanished' ECLOSED.
checkFileClosed :: HasCallStack => File -> (UVFD -> IO a) -> IO a
checkFileClosed (File fd closedRef) f = do
    closed <- readIORef closedRef
    if closed then throwECLOSED else f fd

instance Input File where
    -- readInput :: HasCallStack => File -> Ptr Word8 -> Int -> IO Int
    -- use -1 offset to use fd's default offset
    readInput f buf bufSiz = readFile f buf bufSiz (-1)

-- | Read file with given offset
--
-- Read length may be smaller than buffer size.
readFile :: HasCallStack
           => File
           -> Ptr Word8 -- ^ buffer
           -> Int       -- ^ buffer size
           -> Int64     -- ^ file offset, pass -1 to use default(system) offset
           -> IO Int    -- ^ read length
readFile uvf buf bufSiz off =
    checkFileClosed uvf $ \ fd -> throwUVIfMinus $ hs_uv_fs_read fd buf bufSiz off

instance Output File where
    writeOutput f buf bufSiz = writeFile f buf bufSiz (-1)

-- | Write buffer to file
--
-- This function will loop until all bytes are written.
writeFile :: HasCallStack
            => File
            -> Ptr Word8 -- ^ buffer
            -> Int       -- ^ buffer size
            -> Int64     -- ^ file offset, pass -1 to use default(system) offset
            -> IO ()
writeFile uvf buf0 bufSiz0 off0 =
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
                              -- but only if the file was created, see 'DEFAULT_MODE'.
           -> Resource File
initFile path flags mode =
    initResource
        (do !fd <- withCBytes path $ \ p ->
                throwUVIfMinus $ hs_uv_fs_open p flags mode
            File fd <$> newIORef False)
        (\ (File fd closedRef) -> do
            closed <- readIORef closedRef
            unless closed $ do
                throwUVIfMinus_ (hs_uv_fs_close fd)
                writeIORef closedRef True)

--------------------------------------------------------------------------------

-- | Equivalent to <http://linux.die.net/man/2/mkdir mkdir(2)>.
--
-- Note mode is currently not implemented on Windows.
mkdir :: HasCallStack => CBytes -> FileMode -> IO ()
mkdir path mode = throwUVIfMinus_ . withCBytes path $ \ p ->
     hs_uv_fs_mkdir p mode

-- | Equivalent to <http://linux.die.net/man/2/unlink unlink(2)>.
unlink :: HasCallStack => CBytes -> IO ()
unlink path = throwUVIfMinus_ (withCBytes path hs_uv_fs_unlink)


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
    withCBytes path $ \ p ->
        CBytes.create (size+7) $ \ p' -> do  -- we append "XXXXXX\NUL" in C
            throwUVIfMinus_ (hs_uv_fs_mkdtemp p size p')
            return (size+6)

-- | Equivalent to <http://linux.die.net/man/2/rmdir rmdir(2)>.
rmdir :: HasCallStack => CBytes -> IO ()
rmdir path = throwUVIfMinus_ (withCBytes path hs_uv_fs_rmdir)

-- | Equivalent to <http://linux.die.net/man/3/scandir scandir(3)>.
--
-- Note Unlike scandir(3), this function does not return the “.” and “..” entries.
--
-- Note On Linux, getting the type of an entry is only supported by some file systems (btrfs, ext2, ext3 and ext4 at the time of this writing), check the <http://linux.die.net/man/2/getdents getdents(2)> man page.
scandir :: HasCallStack => CBytes -> IO [(CBytes, DirEntType)]
scandir path = do
    bracket
        (withCBytes path $ \ p ->
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

-- | Equivalent to <http://linux.die.net/man/2/stat stat(2)>
stat :: HasCallStack => CBytes -> IO UVStat
stat path = withCBytes path $ \ p ->
     allocaBytes uvStatSize $ \ s -> do
        throwUVIfMinus_ (hs_uv_fs_stat p s)
        peekUVStat s

-- | Equivalent to <http://linux.die.net/man/2/lstat lstat(2)>
lstat :: HasCallStack => CBytes -> IO UVStat
lstat path = withCBytes path $ \ p ->
     allocaBytes uvStatSize $ \ s -> do
        throwUVIfMinus_ (hs_uv_fs_lstat p s)
        peekUVStat s

-- | Equivalent to <http://linux.die.net/man/2/fstat fstat(2)>
fstat :: HasCallStack => File -> IO UVStat
fstat uvf = checkFileClosed uvf $ \ fd ->
    allocaBytes uvStatSize $ \ s -> do
        throwUVIfMinus_ (hs_uv_fs_fstat fd s)
        peekUVStat s

--------------------------------------------------------------------------------

-- | Equivalent to <http://linux.die.net/man/2/rename rename(2)>.
--
-- Note On Windows if this function fails with UV_EBUSY, UV_EPERM or UV_EACCES, it will retry to rename the file up to four times with 250ms wait between attempts before giving up. If both path and new_path are existing directories this function will work only if target directory is empty.
rename :: HasCallStack => CBytes -> CBytes -> IO ()
rename path path' = throwUVIfMinus_ . withCBytes path $ \ p ->
    withCBytes path' (hs_uv_fs_rename p)

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
copyfile :: HasCallStack => CBytes -> CBytes -> UVCopyFileFlag -> IO ()
copyfile path path' flag = throwUVIfMinus_ . withCBytes path $ \ p ->
    withCBytes path' $ \ p' -> hs_uv_fs_copyfile p p' flag

-- | Equivalent to <http://linux.die.net/man/2/access access(2)> on Unix.
-- Windows uses GetFileAttributesW().
access :: HasCallStack => CBytes -> UVAccessMode -> IO AccessResult
access path mode = do
     r <- withCBytes path $ \ p -> fromIntegral <$> hs_uv_fs_access p mode
     if | r == 0           -> return AccessOK
        | r == UV_ENOENT   -> return NoExistence
        | r == UV_EACCES   -> return NoPermission
        | otherwise        -> do
            name <- uvErrName r
            desc <- uvStdError r
            throwUVError r (IOEInfo name desc callStack)

-- | Equivalent to <http://linux.die.net/man/2/chmod chmod(2)>.
chmod :: HasCallStack => CBytes -> FileMode -> IO ()
chmod path mode = throwUVIfMinus_ . withCBytes path $ \ p -> hs_uv_fs_chmod p mode

-- | Equivalent to <http://linux.die.net/man/2/fchmod fchmod(2)>.
fchmod :: HasCallStack => File -> FileMode -> IO ()
fchmod uvf mode = checkFileClosed uvf $ \ fd -> throwUVIfMinus_ $ hs_uv_fs_fchmod fd mode

-- | Equivalent to <http://linux.die.net/man/2/utime utime(2)>.
--
-- libuv choose 'Double' type due to cross platform concerns, we only provide micro-second precision:
--
--   * second     = v
--   * nanosecond = (v * 1000000) % 1000000 * 1000;
--
-- second and nanosecond are fields in 'UVTimeSpec' respectively.
--
-- Note libuv prior to v1.23.1 have issues which may result in nanosecond not set, 'futime' doesn't have
-- that issue.
utime :: HasCallStack
      => CBytes
      -> Double     -- ^ atime, i.e. access time
      -> Double     -- ^ mtime, i.e. modify time
      -> IO ()
utime path atime mtime = throwUVIfMinus_ . withCBytes path $ \ p -> hs_uv_fs_utime p atime mtime

-- | Equivalent to <http://linux.die.net/man/2/futime futime(2)>.
--
-- Same precision notes with 'utime'.
futime :: HasCallStack => File -> Double -> Double -> IO ()
futime uvf atime mtime = checkFileClosed uvf $ \ fd ->
    throwUVIfMinus_ (hs_uv_fs_futime fd atime mtime)

-- | Equivalent to <http://linux.die.net/man/2/link link(2)>.
link :: HasCallStack => CBytes -> CBytes -> IO ()
link path path' = throwUVIfMinus_ . withCBytes path $ \ p ->
    withCBytes path' $ hs_uv_fs_link p

-- | Equivalent to <http://linux.die.net/man/2/symlink symlink(2)>.
--
-- | Note On Windows the flags parameter can be specified to control how the symlink will be created.
--
--   * 'SYMLINK_DIR': indicates that path points to a directory.
--   * 'SYMLINK_JUNCTION': request that the symlink is created using junction points.
--
-- On other platforms these flags are ignored.
symlink :: HasCallStack => CBytes -> CBytes -> UVSymlinkFlag -> IO ()
symlink path path' flag = throwUVIfMinus_ . withCBytes path $ \ p ->
    withCBytes path' $ \ p' -> hs_uv_fs_symlink p p' flag

-- | Equivalent to <http://linux.die.net/man/2/readlink readlink(2)>.
readlink :: HasCallStack => CBytes -> IO CBytes
readlink path = do
    bracket
        (withCBytes path $ \ p ->
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
        (withCBytes path $ \ p ->
            allocPrimUnsafe $ \ p' ->
                throwUVIfMinus (hs_uv_fs_realpath p p'))
        (hs_uv_fs_readlink_cleanup . fst)
        (fromCString . fst)
