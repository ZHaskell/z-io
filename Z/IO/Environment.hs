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
  , getResUsage
  , getResidentSetMemory
  , getUpTime
  , getHighResolutionTime
  , PID(..)
  , getPID, getPPID
  , getHostname
  , getOSName, OSName(..)
  , getRandom, getRandomT
  ) where

import Control.Monad
import Data.Word
import Z.Data.Vector.Base as V
import Z.Data.CBytes
import Z.Foreign
import Z.IO.Exception
import Z.IO.UV.Errno
import Z.IO.UV.Manager
import Foreign.Storable
import Z.IO.UV.FFI

-- | Computation 'getArgs' returns a list of the program's command
-- line arguments (including the program path).
--
-- This is different from base's 'System.Environment.getArgs' since result
-- includes the program path(more like C's *argv).
getArgs :: IO [CBytes]
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
getEnv' k = getEnv k >>= \ mv -> case mv of
    Just v -> return v
    _ -> throwUVError UV_ENOENT (IOEInfo "ENOENT" "no such environment variable" callStack)

-- | Creates or updates the environment variable specified by name with value.
--
-- Warning: This function is not thread safe.
setEnv :: HasCallStack => CBytes -> CBytes -> IO ()
setEnv k v = withCBytesUnsafe k $ \ p_k ->
    withCBytesUnsafe v $ \ p_v ->
        throwUVIfMinus_ (uv_os_setenv p_k p_v)

-- | Deletes the environment variable specified by name if such environment variable exists.
--
-- Warning: This function is not thread safe.
unsetEnv :: HasCallStack => CBytes -> IO ()
unsetEnv k = void . withCBytesUnsafe k $ \ p -> throwUVIfMinus_ (uv_os_unsetenv p)

-- | Gets the resident set size (RSS) for the current process.
getResidentSetMemory :: HasCallStack => IO CSize
getResidentSetMemory = do
    (size, r) <- allocPrimUnsafe uv_resident_set_memory
    throwUVIfMinus_ (return r)
    return size

-- | Gets the current system uptime.
getUpTime :: HasCallStack => IO Double
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
getHighResolutionTime = uv_hrtime

-- | Gets the resource usage measures for the current process.
--
--  On Windows not all fields are set, the unsupported fields are filled with zeroes.
--  See 'ResUsage' for more details.
getResUsage :: HasCallStack => IO ResUsage
getResUsage = do
    (MutableByteArray mba#) <- newByteArray sizeOfResUsage
    throwUVIfMinus_ (uv_getrusage mba#)
    peekResUsage mba#

-- | Returns the current process ID.
getPID :: IO PID
getPID = uv_os_getpid

-- | Returns the parent process ID.
getPPID :: IO PID
getPPID = uv_os_getppid

-- | Returns the hostname as a null-terminated string.
--
getHostname :: HasCallStack => IO CBytes
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
getRandom siz = do
    (v, _) <- allocPrimVectorUnsafe siz $ \ mba# ->
        throwUVIfMinus_ (hs_uv_random mba# (fromIntegral siz) 0)
    return v

-- | Fill buf with exactly buflen cryptographically strong random bytes acquired from the system CSPRNG.
--
-- The function run 'getRandom' in libuv's threadpool, suitable for get long random byte sequences.
getRandomT :: Int -> IO V.Bytes
getRandomT siz = do
    (v, _) <- allocPrimVectorSafe siz $ \ p -> do
        uvm <- getUVManager
        withUVRequest_ uvm (hs_uv_random_threaded p (fromIntegral siz) 0)
    return v

--------------------------------------------------------------------------------

-- from base
foreign import ccall unsafe getProgArgv :: MBA# CInt -> MBA# (Ptr CString) -> IO ()
