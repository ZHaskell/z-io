{-|
Module      : Z.IO.UV.FFI_Env
Description : libuv operations
Copyright   : (c) Winterland, 2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

INTERNAL MODULE, split from "Z.IO.UV.FFI" to make it buildable under constrained memory.

-}

module Z.IO.UV.FFI_Env where

import           Control.Monad
import           Data.Int
import           Data.Word
import           Data.Primitive.Types   (Prim)
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import           Z.Data.Array.Unaligned
import           Z.Data.Text.Print   (Print(..))
import           Z.Data.JSON         (JSON)
import           Z.Data.CBytes as CBytes
import           Z.Foreign
import           Z.IO.Exception (throwUVIfMinus_, bracket, HasCallStack)
import           GHC.Generics
import           Z.IO.UV.FFI

#include "hs_uv.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

foreign import ccall unsafe uv_resident_set_memory :: MBA## CSize -> IO CInt
foreign import ccall unsafe uv_uptime :: MBA## Double -> IO CInt
foreign import ccall unsafe uv_getrusage :: MBA## a -> IO CInt

foreign import ccall unsafe uv_get_free_memory :: IO Word64
foreign import ccall unsafe uv_get_total_memory :: IO Word64
foreign import ccall unsafe uv_get_constrained_memory :: IO Word64

-- | Data type for storing times.
-- typedef struct { long tv_sec; long tv_usec; } uv_timeval_t;
data TimeVal = TimeVal
    { tv_sec  :: {-# UNPACK #-} !CLong
    , tv_usec :: {-# UNPACK #-} !CLong
    }   deriving (Show, Read, Eq, Ord, Generic)
        deriving anyclass (Print, JSON)

-- | Data type for resource usage results.
--
-- Members marked with (X) are unsupported on Windows.
-- See <https://man7.org/linux/man-pages/man2/getrusage.2.html getrusage(2)> for supported fields on Unix
data ResUsage = ResUsage
    { ru_utime    :: {-# UNPACK #-} !TimeVal   -- ^  user CPU time used, in microseconds
    , ru_stime    :: {-# UNPACK #-} !TimeVal   -- ^  system CPU time used, in microseconds
    , ru_maxrss   :: {-# UNPACK #-} !Word64    -- ^  maximum resident set size
    , ru_ixrss    :: {-# UNPACK #-} !Word64    -- ^  integral shared memory size (X)
    , ru_idrss    :: {-# UNPACK #-} !Word64    -- ^  integral unshared data size (X)
    , ru_isrss    :: {-# UNPACK #-} !Word64    -- ^  integral unshared stack size (X)
    , ru_minflt   :: {-# UNPACK #-} !Word64    -- ^  page reclaims (soft page faults) (X)
    , ru_majflt   :: {-# UNPACK #-} !Word64    -- ^  page faults (hard page faults)
    , ru_nswap    :: {-# UNPACK #-} !Word64    -- ^  swaps (X)
    , ru_inblock  :: {-# UNPACK #-} !Word64    -- ^  block input operations
    , ru_oublock  :: {-# UNPACK #-} !Word64    -- ^  block output operations
    , ru_msgsnd   :: {-# UNPACK #-} !Word64    -- ^  IPC messages sent (X)
    , ru_msgrcv   :: {-# UNPACK #-} !Word64    -- ^  IPC messages received (X)
    , ru_nsignals :: {-# UNPACK #-} !Word64    -- ^  signals received (X)
    , ru_nvcsw    :: {-# UNPACK #-} !Word64    -- ^  voluntary context switches (X)
    , ru_nivcsw   :: {-# UNPACK #-} !Word64    -- ^  involuntary context switches (X)
    }   deriving (Show, Read, Eq, Ord, Generic)
        deriving anyclass (Print, JSON)

sizeOfResUsage :: Int
{-# INLINABLE sizeOfResUsage #-}
sizeOfResUsage = #size uv_rusage_t

peekResUsage :: MBA## a -> IO ResUsage
{-# INLINABLE peekResUsage #-}
peekResUsage mba = do
    utime_sec :: CLong <- peekMBA mba (#offset  uv_rusage_t, ru_utime )
    utime_usec :: CLong <- peekMBA mba ((#offset  uv_rusage_t, ru_utime) + sizeOf (undefined :: CLong))
    stime_sec :: CLong <- peekMBA mba (#offset  uv_rusage_t, ru_stime )
    stime_usec :: CLong <- peekMBA mba ((#offset  uv_rusage_t, ru_stime) + sizeOf (undefined :: CLong))
    maxrss   <- peekMBA mba (#offset  uv_rusage_t, ru_maxrss  )
    ixrss    <- peekMBA mba (#offset  uv_rusage_t, ru_ixrss   )
    idrss    <- peekMBA mba (#offset  uv_rusage_t, ru_idrss   )
    isrss    <- peekMBA mba (#offset  uv_rusage_t, ru_isrss   )
    minflt   <- peekMBA mba (#offset  uv_rusage_t, ru_minflt  )
    majflt   <- peekMBA mba (#offset  uv_rusage_t, ru_majflt  )
    nswap    <- peekMBA mba (#offset  uv_rusage_t, ru_nswap   )
    inblock  <- peekMBA mba (#offset  uv_rusage_t, ru_inblock )
    oublock  <- peekMBA mba (#offset  uv_rusage_t, ru_oublock )
    msgsnd   <- peekMBA mba (#offset  uv_rusage_t, ru_msgsnd  )
    msgrcv   <- peekMBA mba (#offset  uv_rusage_t, ru_msgrcv  )
    nsignals <- peekMBA mba (#offset  uv_rusage_t, ru_nsignals)
    nvcsw    <- peekMBA mba (#offset  uv_rusage_t, ru_nvcsw   )
    nivcsw   <- peekMBA mba (#offset  uv_rusage_t, ru_nivcsw  )
    return (ResUsage (TimeVal utime_sec utime_usec) (TimeVal stime_sec stime_usec)
                    maxrss ixrss idrss isrss minflt majflt nswap inblock
                    oublock msgsnd msgrcv nsignals nvcsw nivcsw)

foreign import ccall unsafe uv_os_getpid :: IO PID
foreign import ccall unsafe uv_os_getppid :: IO PID
foreign import ccall unsafe uv_os_getpriority :: PID -> MBA## CInt -> IO CInt
foreign import ccall unsafe uv_os_setpriority :: PID -> CInt -> IO CInt

newtype PID = PID CInt
    deriving (Eq, Ord, Show, Read, Generic)
    deriving newtype (Storable, Prim, Unaligned, JSON)
    deriving anyclass Print

type Priority = CInt
pattern PRIORITY_LOW          :: Priority
pattern PRIORITY_BELOW_NORMAL :: Priority
pattern PRIORITY_NORMAL       :: Priority
pattern PRIORITY_ABOVE_NORMAL :: Priority
pattern PRIORITY_HIGH         :: Priority
pattern PRIORITY_HIGHEST      :: Priority
pattern PRIORITY_LOW           = #const UV_PRIORITY_LOW
pattern PRIORITY_BELOW_NORMAL  = #const UV_PRIORITY_BELOW_NORMAL
pattern PRIORITY_NORMAL        = #const UV_PRIORITY_NORMAL
pattern PRIORITY_ABOVE_NORMAL  = #const UV_PRIORITY_ABOVE_NORMAL
pattern PRIORITY_HIGH          = #const UV_PRIORITY_HIGH
pattern PRIORITY_HIGHEST       = #const UV_PRIORITY_HIGHEST

foreign import ccall unsafe uv_hrtime :: IO Word64

foreign import ccall unsafe uv_os_environ :: MBA## (Ptr a) -> MBA## CInt -> IO CInt
foreign import ccall unsafe uv_os_free_environ :: Ptr a -> CInt -> IO ()
foreign import ccall unsafe uv_os_getenv :: BA## Word8 -> MBA## Word8 -> MBA## CSize -> IO CInt
foreign import ccall unsafe uv_os_setenv :: BA## Word8 -> BA## Word8 -> IO CInt
foreign import ccall unsafe uv_os_unsetenv :: BA## Word8 -> IO CInt

pattern UV_MAXHOSTNAMESIZE :: CSize
pattern UV_MAXHOSTNAMESIZE = #const UV_MAXHOSTNAMESIZE
foreign import ccall unsafe uv_os_gethostname :: MBA## Word8 -> MBA## CSize -> IO CInt

-- | Data type for operating system name and version information.
data OSName = OSName
    { os_sysname :: CBytes
    , os_release :: CBytes
    , os_version :: CBytes
    , os_machine :: CBytes
    }   deriving (Eq, Ord, Show, Read, Generic)
        deriving anyclass (Print, JSON)

getOSName :: HasCallStack => IO OSName
{-# INLINABLE getOSName #-}
getOSName = do
    (MutableByteArray mba##) <- newByteArray (#size uv_utsname_t)
    throwUVIfMinus_ (uv_os_uname mba##)
    sn <- peekMBACBytes mba## (#offset uv_utsname_t, sysname)
    re <- peekMBACBytes mba## (#offset uv_utsname_t, release)
    ve <- peekMBACBytes mba## (#offset uv_utsname_t, version)
    ma <- peekMBACBytes mba##  (#offset uv_utsname_t, machine)
    return (OSName sn re ve ma)

foreign import ccall unsafe uv_os_uname :: MBA## OSName -> IO CInt

foreign import ccall unsafe hs_uv_random :: MBA## Word8 -> CSize -> CInt -> IO CInt
foreign import ccall unsafe hs_uv_random_threaded :: Ptr Word8 -> CSize -> CInt -> Ptr UVLoop -> IO UVSlotUnsafe

-- | Data type for password file information.
data PassWD = PassWD
    { passwd_username :: CBytes
    , passwd_uid :: UID
    , passwd_gid :: GID
    , passwd_shell :: CBytes
    , passwd_homedir :: CBytes
    }   deriving (Eq, Ord, Show, Read, Generic)
        deriving anyclass (Print, JSON)

foreign import ccall unsafe uv_os_get_passwd :: MBA## PassWD -> IO CInt
foreign import ccall unsafe uv_os_free_passwd :: MBA## PassWD -> IO ()

-- | Gets a subset of the password file entry for the current effective uid (not the real uid).
--
-- The populated data includes the username, euid, gid, shell, and home directory.
-- On non-Windows systems, all data comes from getpwuid_r(3).
-- On Windows, uid and gid are set to -1 and have no meaning, and shell is empty.
getPassWD :: HasCallStack => IO PassWD
{-# INLINABLE getPassWD #-}
getPassWD =  bracket
    (do mpa@(MutableByteArray mba##) <- newByteArray (#size uv_passwd_t)
        throwUVIfMinus_ (uv_os_get_passwd mba##)
        return mpa)
    (\ (MutableByteArray mba##) -> uv_os_free_passwd mba##)
    (\ (MutableByteArray mba##) -> do
        username <- fromCString =<< peekMBA mba## (#offset uv_passwd_t, username)
        uid <- fromIntegral <$> (peekMBA mba## (#offset uv_passwd_t, uid) :: IO CLong)
        gid <- fromIntegral <$> (peekMBA mba## (#offset uv_passwd_t, gid) :: IO CLong)
        shell <- fromCString =<< peekMBA mba## (#offset uv_passwd_t, shell)
        homedir <- fromCString =<< peekMBA mba## (#offset uv_passwd_t, homedir)
        return (PassWD username uid gid shell homedir))

foreign import ccall unsafe uv_cwd :: MBA## Word8 -> MBA## CSize -> IO CInt
foreign import ccall unsafe uv_chdir :: BA## Word8 -> IO CInt
foreign import ccall unsafe uv_os_homedir :: MBA## Word8 -> MBA## CSize -> IO CInt
foreign import ccall unsafe uv_os_tmpdir :: MBA## Word8 -> MBA## CSize -> IO CInt

foreign import ccall unsafe uv_cpu_info      :: MBA## (Ptr CPUInfo) -> MBA## CInt -> IO CInt
foreign import ccall unsafe uv_free_cpu_info :: Ptr CPUInfo -> CInt -> IO ()

-- | Data type for CPU information.
data CPUInfo = CPUInfo
    { cpu_model :: CBytes
    , cpu_speed :: CInt
    , cpu_times_user :: Word64  -- ^ milliseconds
    , cpu_times_nice :: Word64  -- ^ milliseconds
    , cpu_times_sys  :: Word64  -- ^ milliseconds
    , cpu_times_idle :: Word64  -- ^ milliseconds
    , cpu_times_irq  :: Word64  -- ^ milliseconds
    }   deriving (Eq, Ord, Show, Read, Generic)
        deriving anyclass (Print, JSON)

-- | Gets information about the CPUs on the system.
getCPUInfo :: HasCallStack => IO [CPUInfo]
{-# INLINABLE getCPUInfo #-}
getCPUInfo = bracket
    (do (p, (len, _)) <-  allocPrimUnsafe $ \ pp ->
            allocPrimUnsafe $ \ plen ->
                throwUVIfMinus_ (uv_cpu_info pp plen)
        return (p, len))
    (\ (p, len) -> uv_free_cpu_info p len)
    (\ (p, len) -> forM [0..fromIntegral len-1] (peekCPUInfoOff p))

peekCPUInfoOff :: Ptr CPUInfo -> Int -> IO CPUInfo
{-# INLINABLE peekCPUInfoOff #-}
peekCPUInfoOff p off = do
    let p' = p `plusPtr` (off * (#size uv_cpu_info_t))
    model <- fromCString =<< (#peek uv_cpu_info_t, model) p'
    speed <- (#peek uv_cpu_info_t, speed) p'
    user <- (#peek uv_cpu_info_t, cpu_times.user) p'
    nice <- (#peek uv_cpu_info_t, cpu_times.nice) p'
    sys <- (#peek uv_cpu_info_t, cpu_times.sys) p'
    idle <- (#peek uv_cpu_info_t, cpu_times.idle) p'
    irq <- (#peek uv_cpu_info_t, cpu_times.irq) p'
    return (CPUInfo model speed user nice sys idle irq)

foreign import ccall unsafe uv_loadavg :: MBA## (Double, Double, Double) -> IO ()

-- | Gets the load average. See: <https://en.wikipedia.org/wiki/Load_(computing)>
getLoadAvg :: IO (Double, Double, Double)
{-# INLINABLE getLoadAvg #-}
getLoadAvg = do
    (arr, _) <- allocPrimArrayUnsafe 3 uv_loadavg
    return ( indexPrimArray arr 0
           , indexPrimArray arr 1
           , indexPrimArray arr 2)

-- | Alternative data type for storing times.
-- typedef struct { int64_t tv_sec; int32_t tv_usec; } uv_timeval64_t;
data TimeVal64 = TimeVal64
    { tv64_sec  :: {-# UNPACK #-} !Int64
    , tv64_usec :: {-# UNPACK #-} !Int32
    }   deriving (Show, Read, Eq, Ord, Generic)
        deriving anyclass (Print, JSON)

foreign import ccall unsafe uv_gettimeofday :: MBA## TimeVal64 -> IO CInt

-- | Cross-platform implementation of <https://man7.org/linux/man-pages/man2/gettimeofday.2.html gettimeofday(2)>.
-- The timezone argument to gettimeofday() is not supported, as it is considered obsolete.
getTimeOfDay :: HasCallStack => IO TimeVal64
{-# INLINABLE getTimeOfDay #-}
getTimeOfDay = do
    (MutableByteArray mba##) <- newByteArray (#size uv_timeval64_t)
    throwUVIfMinus_ (uv_gettimeofday mba##)
    s <- peekMBA mba## (#offset uv_timeval64_t, tv_sec)
    us <- peekMBA mba## (#offset uv_timeval64_t, tv_usec)
    return (TimeVal64 s us)
