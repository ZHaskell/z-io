{-|
Module      : Z.IO.Environment
Description : Miscellaneous functions(environment variables, metrics, etc.)
Copyright   : (c) Dong Han, 2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide methods for retrieving various environment infomation. There's no encoding guarantee about these information, if you want textual representation, UTF8 assumption is recommended. i.e. use 'Z.Data.Text.validate'.

-}
module Z.IO.Environment
  ( -- * arguments
    getArgs
    -- * environment variables
  , getAllEnv
  , getEnv, getEnv'
  , setEnv, unsetEnv
    -- * other environment infos
  , getCWD, chDir, getHomeDir, getTempDir
  , getRandom, getRandomT
  , getResUsage, ResUsage(..), TimeVal(..)
  , getResidentSetMemory
  , getUpTime
  , getHighResolutionTime
  , PID(..)
  , getPID, getPPID
  , getHostname
  , getOSName, OSName(..)
  , getPassWD, PassWD(..), UID, GID
  , getCPUInfo, CPUInfo(..)
  , getLoadAvg
  , getFreeMem, getTotalMem, getConstrainedMem
  ) where

import Control.Monad
import Data.Word
import qualified Z.Data.Vector.Base as V
import Z.Data.CBytes
import Z.Foreign
import Z.IO.Exception
import Z.IO.UV.Manager
import Foreign.Storable
import Z.IO.UV.FFI
import Z.IO.UV.FFI_Env

-- | Computation 'getArgs' returns a list of the program's command
-- line arguments (including the program path).
--
-- This is different from base's 'System.Environment.getArgs' since result
-- includes the program path(more like C's *argv).
getArgs :: IO [CBytes]
{-# INLINABLE getArgs #-}
getArgs = do
    (argc :: CInt, (p_argv :: Ptr CString, _)) <- allocPrimUnsafe $ \ p_argc -> do
        allocPrimUnsafe $ \ p_p_argv -> do
            getProgArgv p_argc p_p_argv
    forM [0..fromIntegral (argc-1)] $ \ i -> do
        fromCString =<< peekElemOff p_argv i

-- | Retrieves the environment variable.
--
-- Warning: This function is not thread safe.
getAllEnv :: HasCallStack => IO [(CBytes, CBytes)]
{-# INLINABLE getAllEnv #-}
getAllEnv = bracket
    (do (p_env :: Ptr CString, (envc :: CInt, _)) <- allocPrimUnsafe $ \ p_p_env -> do
            allocPrimUnsafe $ \ p_envc ->
                throwUVIfMinus_ (uv_os_environ p_p_env p_envc)
        return (p_env, envc))
    (\ (p_env, envc) -> uv_os_free_environ p_env envc)
    (\ (p_env, envc) -> do
        forM [0..fromIntegral (envc-1)] $ \ i -> do
            k <- fromCString =<< peekElemOff p_env (i*2)
            v <- fromCString =<< peekElemOff p_env (i*2+1)
            return (k, v))

-- | Retrieves the environment variable specified by name.
--
-- Warning: This function is not thread safe.
getEnv :: HasCallStack => CBytes -> IO (Maybe CBytes)
{-# INLINABLE getEnv #-}
getEnv k = go 512
  where
    go siz = do
        (siz', (v, r))<- withPrimUnsafe siz $ \ p_siz ->
            withCBytesUnsafe k $ \ p_k ->
                allocCBytesUnsafe siz $ \ p_v ->
                    uv_os_getenv p_k p_v p_siz
        case r of
            UV_ENOBUFS -> go siz'
            UV_ENOENT -> return Nothing
            _ -> do
                throwUVIfMinus_ (return r)
                return (Just v)

-- | Retrieves the environment variable specified by name, throw 'NoSuchThing' if not exists.
--
-- Warning: This function is not thread safe.
getEnv' :: HasCallStack => CBytes -> IO CBytes
{-# INLINABLE getEnv' #-}
getEnv' k = getEnv k >>= \ mv -> case mv of
    Just v -> return v
    _ -> throwUVError UV_ENOENT (IOEInfo "ENOENT" "no such environment variable" callStack)

-- | Creates or updates the environment variable specified by name with value.
--
-- Warning: This function is not thread safe.
setEnv :: HasCallStack => CBytes -> CBytes -> IO ()
{-# INLINABLE setEnv #-}
setEnv k v = withCBytesUnsafe k $ \ p_k ->
    withCBytesUnsafe v $ \ p_v ->
        throwUVIfMinus_ (uv_os_setenv p_k p_v)

-- | Deletes the environment variable specified by name if such environment variable exists.
--
-- Warning: This function is not thread safe.
unsetEnv :: HasCallStack => CBytes -> IO ()
{-# INLINABLE unsetEnv #-}
unsetEnv k = void . withCBytesUnsafe k $ \ p -> throwUVIfMinus_ (uv_os_unsetenv p)

-- | Gets the resident set size (RSS) for the current process.
getResidentSetMemory :: HasCallStack => IO CSize
{-# INLINABLE getResidentSetMemory #-}
getResidentSetMemory = do
    (size, r) <- allocPrimUnsafe uv_resident_set_memory
    throwUVIfMinus_ (return r)
    return size

-- | Gets the current system uptime.
getUpTime :: HasCallStack => IO Double
{-# INLINABLE getUpTime #-}
getUpTime = do
    (size, r) <- allocPrimUnsafe uv_uptime
    throwUVIfMinus_ (return r)
    return size

-- | Returns the current high-resolution real time.
--
-- This is expressed in nanoseconds. It is relative to an arbitrary time in the past.
-- It is not related to the time of day and therefore not subject to clock drift.
-- The primary use is for measuring performance between intervals.
getHighResolutionTime :: IO Word64
{-# INLINABLE getHighResolutionTime #-}
getHighResolutionTime = uv_hrtime

-- | Gets the resource usage measures for the current process.
--
--  On Windows not all fields are set, the unsupported fields are filled with zeroes.
--  See 'ResUsage' for more details.
getResUsage :: HasCallStack => IO ResUsage
{-# INLINABLE getResUsage #-}
getResUsage = do
    (MutableByteArray mba#) <- newByteArray sizeOfResUsage
    throwUVIfMinus_ (uv_getrusage mba#)
    peekResUsage mba#

-- | Returns the current process ID.
getPID :: IO PID
{-# INLINABLE getPID #-}
getPID = uv_os_getpid

-- | Returns the parent process ID.
getPPID :: IO PID
{-# INLINABLE getPPID #-}
getPPID = uv_os_getppid

-- | Returns the hostname as a null-terminated string.
--
getHostname :: HasCallStack => IO CBytes
{-# INLINABLE getHostname #-}
getHostname = do
    (n, _) <- allocCBytesUnsafe (fromIntegral UV_MAXHOSTNAMESIZE) $ \ p_n ->
        withPrimUnsafe UV_MAXHOSTNAMESIZE $ \ p_siz ->
            throwUVIfMinus_ (uv_os_gethostname p_n p_siz)
    return n

-- | Fill buf with exactly buflen cryptographically strong random bytes acquired from the system CSPRNG.
--
-- The function may block indefinitely when not enough entropy is available, don't use it to get
-- long random sequences.
getRandom :: Int -> IO V.Bytes
{-# INLINABLE getRandom #-}
getRandom siz = do
    (v, _) <- allocPrimVectorUnsafe siz $ \ mba# ->
        throwUVIfMinus_ (hs_uv_random mba# (fromIntegral siz) 0)
    return v

-- | Fill buf with exactly buflen cryptographically strong random bytes acquired from the system CSPRNG.
--
-- The function run 'getRandom' in libuv's threadpool, suitable for get long random byte sequences.
getRandomT :: Int -> IO V.Bytes
{-# INLINABLE getRandomT #-}
getRandomT siz = do
    (v, _) <- allocPrimVectorSafe siz $ \ p -> do
        uvm <- getUVManager
        withUVRequest_ uvm (hs_uv_random_threaded p (fromIntegral siz) 0)
    return v

-- | Gets the current working directory.
--
getCWD :: HasCallStack => IO CBytes
{-# INLINABLE getCWD #-}
getCWD = go 512
  where
    go siz = do
        (siz', (v, r))<- withPrimUnsafe siz $ \ p_siz ->
            allocCBytesUnsafe siz $ \ p_v ->
                uv_cwd p_v p_siz
        case r of
            UV_ENOBUFS -> go siz'
            _ -> do
                throwUVIfMinus_ (return r)
                return v

-- | Changes the current working directory.
--
chDir :: HasCallStack => CBytes -> IO ()
{-# INLINABLE chDir #-}
chDir p = throwUVIfMinus_ (withCBytesUnsafe p $ \ pp -> uv_chdir pp)

-- | Gets the current user’s home directory.
--
-- On Windows, first checks the USERPROFILE environment variable using GetEnvironmentVariableW().
-- If USERPROFILE is not set, GetUserProfileDirectoryW() is called.
-- On all other operating systems, first checks the HOME environment variable using getenv(3).
-- If HOME is not set, getpwuid_r(3) is called.
--
-- Warning 'getHomeDir' is not thread safe.
getHomeDir :: HasCallStack => IO CBytes
{-# INLINABLE getHomeDir #-}
getHomeDir = go 512
  where
    go siz = do
        (siz', (v, r))<- withPrimUnsafe siz $ \ p_siz ->
            allocCBytesUnsafe siz $ \ p_v ->
                uv_os_homedir p_v p_siz
        case r of
            UV_ENOBUFS -> go siz'
            _ -> do
                throwUVIfMinus_ (return r)
                return v

-- | Gets the temp directory.
--
-- On Windows, uses GetTempPathW(). On all other operating systems,
-- uses the first environment variable found in the ordered list TMPDIR, TMP, TEMP, and TEMPDIR.
-- If none of these are found, the path @\/tmp@ is used, or, on Android, @\/data\/local\/tmp@ is used.
--
-- Warning 'getHomeDir' is not thread safe.
getTempDir :: HasCallStack => IO CBytes
{-# INLINABLE getTempDir #-}
getTempDir = go 512
  where
    go siz = do
        (siz', (v, r))<- withPrimUnsafe siz $ \ p_siz ->
            allocCBytesUnsafe siz $ \ p_v ->
                uv_os_tmpdir p_v p_siz
        case r of
            UV_ENOBUFS -> go siz'
            _ -> do
                throwUVIfMinus_ (return r)
                return v

-- | Gets the amount of free memory available in the system, as reported by the kernel (in bytes).
getFreeMem :: IO Word64
{-# INLINABLE getFreeMem #-}
getFreeMem = uv_get_free_memory

-- | Gets the total amount of physical memory in the system (in bytes).
getTotalMem :: IO Word64
{-# INLINABLE getTotalMem #-}
getTotalMem = uv_get_total_memory

-- | Gets the amount of memory available to the process (in bytes) based on limits imposed by the OS.
--
-- If there is no such constraint, or the constraint is unknown, 0 is returned.
-- Note that it is not unusual for this value to be less than or greater than 'getTotalMem'.
-- Note This function currently only returns a non-zero value on Linux, based on cgroups if it is present.
getConstrainedMem :: IO Word64
{-# INLINABLE getConstrainedMem #-}
getConstrainedMem = uv_get_constrained_memory

--------------------------------------------------------------------------------

-- from base
foreign import ccall unsafe getProgArgv :: MBA# CInt -> MBA# (Ptr CString) -> IO ()
