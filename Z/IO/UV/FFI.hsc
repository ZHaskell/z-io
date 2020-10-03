{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE UnliftedFFITypes           #-}

{-|
Module      : Z.IO.UV
Description : libuv operations
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

INTERNAL MODULE, provides all libuv side operations.

-}

module Z.IO.UV.FFI where

import           Data.Bits
import           Data.Int
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import           Z.Data.Array.UnalignedAccess
import qualified Z.Data.Array  as A 
import qualified Z.Data.Vector as V
import qualified Z.Data.Vector.Base as V
import qualified Z.Data.Text   as T
import           Z.Foreign
import           Z.IO.Exception (throwUVIfMinus_)
import           Z.IO.Network.SocketAddr    (SocketAddr)
import           System.Posix.Types (CSsize (..))
import           GHC.Generics

#include "hs_uv.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

--------------------------------------------------------------------------------
-- libuv version
foreign import ccall unsafe uv_version :: IO CUInt
foreign import ccall unsafe uv_version_string :: IO CString

--------------------------------------------------------------------------------
-- Type alias
type UVSlot = Int
-- | UVSlotUnsafe wrap a slot which may not have a 'MVar' in blocking table, 
--   i.e. the blocking table need to be resized.
newtype UVSlotUnsafe = UVSlotUnsafe { unsafeGetSlot :: UVSlot }
type UVFD = Int32

--------------------------------------------------------------------------------
-- CONSTANT

pattern SO_REUSEPORT_LOAD_BALANCE :: Int
pattern SO_REUSEPORT_LOAD_BALANCE = #const SO_REUSEPORT_LOAD_BALANCE
pattern INIT_LOOP_SIZE :: Int
pattern INIT_LOOP_SIZE = #const INIT_LOOP_SIZE

--------------------------------------------------------------------------------
-- loop
data UVLoop
data UVLoopData

peekUVEventQueue :: Ptr UVLoopData -> IO (Int, Ptr Int)
peekUVEventQueue p = (,)
    <$> (#{peek hs_loop_data, event_counter          } p)
    <*> (#{peek hs_loop_data, event_queue            } p)

clearUVEventCounter :: Ptr UVLoopData -> IO ()
clearUVEventCounter p = do
    #{poke hs_loop_data, event_counter          } p $ (0 :: Int)

peekUVBufferTable :: Ptr UVLoopData -> IO (Ptr (Ptr Word8), Ptr CSsize)
peekUVBufferTable p = (,)
    <$> (#{peek hs_loop_data, buffer_table          } p)
    <*> (#{peek hs_loop_data, buffer_size_table     } p)

newtype UVRunMode = UVRunMode CInt deriving (Show, Eq, Ord)

pattern UV_RUN_DEFAULT :: UVRunMode
pattern UV_RUN_DEFAULT = UVRunMode #{const UV_RUN_DEFAULT}
pattern UV_RUN_ONCE :: UVRunMode
pattern UV_RUN_ONCE    = UVRunMode #{const UV_RUN_ONCE}
pattern UV_RUN_NOWAIT :: UVRunMode
pattern UV_RUN_NOWAIT  = UVRunMode #{const UV_RUN_NOWAIT}

-- | Peek loop data pointer from uv loop  pointer.
peekUVLoopData :: Ptr UVLoop -> IO (Ptr UVLoopData)
peekUVLoopData p = #{peek uv_loop_t, data} p

foreign import ccall unsafe hs_uv_loop_init      :: Int -> IO (Ptr UVLoop)
foreign import ccall unsafe hs_uv_loop_close     :: Ptr UVLoop -> IO ()

-- | uv_run with usafe FFI.
foreign import ccall unsafe "hs_uv_run" uv_run    :: Ptr UVLoop -> UVRunMode -> IO CInt

-- | uv_run with safe FFI.
foreign import ccall safe "hs_uv_run" uv_run_safe :: Ptr UVLoop -> UVRunMode -> IO CInt

foreign import ccall unsafe uv_loop_alive :: Ptr UVLoop -> IO CInt

--------------------------------------------------------------------------------
-- thread safe wake up

foreign import ccall unsafe hs_uv_wake_up_timer :: Ptr UVLoopData -> IO CInt
foreign import ccall unsafe hs_uv_wake_up_async :: Ptr UVLoopData -> IO CInt

--------------------------------------------------------------------------------
-- handle
data UVHandle

peekUVHandleData :: Ptr UVHandle -> IO UVSlotUnsafe
peekUVHandleData p =  UVSlotUnsafe <$> (#{peek uv_handle_t, data} p :: IO Int)

foreign import ccall unsafe hs_uv_fileno :: Ptr UVHandle -> IO UVFD
foreign import ccall unsafe hs_uv_handle_alloc :: Ptr UVLoop -> IO (Ptr UVHandle)
foreign import ccall unsafe hs_uv_handle_free  :: Ptr UVHandle -> IO ()
foreign import ccall unsafe hs_uv_handle_close :: Ptr UVHandle -> IO ()

--------------------------------------------------------------------------------
-- request

foreign import ccall unsafe hs_uv_cancel :: Ptr UVLoop -> UVSlot -> IO ()

--------------------------------------------------------------------------------
-- stream

foreign import ccall unsafe hs_uv_listen  :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe hs_uv_listen_resume :: Ptr UVHandle -> IO ()

foreign import ccall unsafe hs_uv_read_start :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe uv_read_stop :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe hs_uv_write :: Ptr UVHandle -> Ptr Word8 -> Int -> IO UVSlotUnsafe

foreign import ccall unsafe hs_uv_accept_check_alloc :: Ptr UVHandle -> IO (Ptr UVHandle)
foreign import ccall unsafe hs_uv_accept_check_init :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe hs_uv_accept_check_close :: Ptr UVHandle -> IO ()

--------------------------------------------------------------------------------
-- tcp & pipe
foreign import ccall unsafe hs_uv_tcp_open :: Ptr UVHandle -> UVFD -> IO CInt
foreign import ccall unsafe uv_tcp_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt
foreign import ccall unsafe uv_tcp_init_ex :: Ptr UVLoop -> Ptr UVHandle -> CUInt -> IO CInt
foreign import ccall unsafe uv_tcp_nodelay :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_tcp_keepalive :: Ptr UVHandle -> CInt -> CUInt -> IO CInt
foreign import ccall unsafe uv_tcp_getsockname :: Ptr UVHandle -> MBA## SocketAddr -> MBA## CInt -> IO CInt
foreign import ccall unsafe uv_tcp_getpeername :: Ptr UVHandle -> MBA## SocketAddr -> MBA## CInt -> IO CInt

uV_TCP_IPV6ONLY :: CUInt
uV_TCP_IPV6ONLY = #{const UV_TCP_IPV6ONLY}

foreign import ccall unsafe uv_tcp_bind :: Ptr UVHandle -> MBA## SocketAddr -> CUInt -> IO CInt
foreign import ccall unsafe hs_uv_tcp_connect :: Ptr UVHandle -> MBA## SocketAddr -> IO UVSlotUnsafe
foreign import ccall unsafe hs_set_socket_reuse :: Ptr UVHandle -> IO CInt

foreign import ccall unsafe hs_uv_pipe_open :: Ptr UVHandle -> UVFD -> IO CInt
foreign import ccall unsafe uv_pipe_init :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_pipe_bind :: Ptr UVHandle -> CString -> IO CInt
foreign import ccall unsafe hs_uv_pipe_connect :: Ptr UVHandle -> CString -> IO UVSlotUnsafe

--------------------------------------------------------------------------------
-- udp
foreign import ccall unsafe uv_udp_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt
foreign import ccall unsafe uv_udp_init_ex :: Ptr UVLoop -> Ptr UVHandle -> CUInt -> IO CInt
foreign import ccall unsafe uv_udp_open :: Ptr UVHandle -> UVFD -> IO CInt
foreign import ccall unsafe uv_udp_bind :: Ptr UVHandle -> MBA## SocketAddr -> UDPFlag -> IO CInt

newtype Membership = Membership CInt deriving (Show, Eq, Ord)

pattern LEAVE_GROUP :: Membership
pattern LEAVE_GROUP = Membership #{const UV_LEAVE_GROUP}
pattern JOIN_GROUP :: Membership
pattern JOIN_GROUP = Membership #{const UV_JOIN_GROUP}

newtype UDPFlag = UDPFlag CInt deriving (Show, Eq, Ord, Storable, Bits, FiniteBits, Num)

pattern UDP_DEFAULT        :: UDPFlag
pattern UDP_DEFAULT         = UDPFlag 0
pattern UDP_IPV6ONLY       :: UDPFlag
pattern UDP_IPV6ONLY        = UDPFlag #{const UV_UDP_IPV6ONLY}
pattern UDP_REUSEADDR      :: UDPFlag
pattern UDP_REUSEADDR       = UDPFlag #{const UV_UDP_REUSEADDR}

pattern UV_UDP_PARTIAL     :: Int32
pattern UV_UDP_PARTIAL      = #{const UV_UDP_PARTIAL}

foreign import ccall unsafe uv_udp_connect
    :: Ptr UVHandle -> MBA## SocketAddr -> IO CInt
-- | Just pass null pointer as SocketAddr to disconnect
foreign import ccall unsafe "uv_udp_connect" uv_udp_disconnect
    :: Ptr UVHandle -> Ptr SocketAddr -> IO CInt

foreign import ccall unsafe uv_udp_set_membership ::
    Ptr UVHandle -> CString -> CString -> Membership -> IO CInt
foreign import ccall unsafe uv_udp_set_source_membership ::
    Ptr UVHandle -> CString -> CString -> CString -> Membership -> IO CInt

foreign import ccall unsafe uv_udp_set_multicast_loop :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_udp_set_multicast_ttl :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_udp_set_multicast_interface :: Ptr UVHandle -> CString -> IO CInt
foreign import ccall unsafe uv_udp_set_broadcast :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_udp_set_ttl :: Ptr UVHandle -> CInt -> IO CInt

foreign import ccall unsafe hs_uv_udp_recv_start :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe uv_udp_recv_stop :: Ptr UVHandle -> IO CInt

foreign import ccall unsafe hs_uv_udp_check_alloc :: Ptr UVHandle -> IO (Ptr UVHandle)
foreign import ccall unsafe hs_uv_udp_check_init :: Ptr UVHandle -> IO CInt
foreign import ccall unsafe hs_uv_udp_check_close :: Ptr UVHandle -> IO ()

foreign import ccall unsafe hs_uv_udp_send 
    :: Ptr UVHandle -> MBA## SocketAddr -> Ptr Word8 -> Int -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_udp_send_connected
    :: Ptr UVHandle -> Ptr Word8 -> Int -> IO UVSlotUnsafe
foreign import ccall unsafe uv_udp_getsockname 
    :: Ptr UVHandle -> MBA## SocketAddr -> MBA## CInt -> IO CInt
foreign import ccall unsafe uv_udp_getpeername
    :: Ptr UVHandle -> MBA## SocketAddr -> MBA## CInt -> IO CInt


--------------------------------------------------------------------------------
-- tty

-- | Terminal mode.
--
-- When in 'UV_TTY_MODE_RAW' mode, input is always available character-by-character,
-- not including modifiers. Additionally, all special processing of characters by the terminal is disabled, 
-- including echoing input characters. Note that CTRL+C will no longer cause a SIGINT when in this mode.
newtype TTYMode = TTYMode CInt
    deriving (Eq, Ord, Show, FiniteBits, Bits, Storable, Num)

pattern TTY_MODE_NORMAL :: TTYMode
pattern TTY_MODE_NORMAL = TTYMode #{const UV_TTY_MODE_NORMAL}
pattern TTY_MODE_RAW :: TTYMode
pattern TTY_MODE_RAW = TTYMode #{const UV_TTY_MODE_RAW}
pattern TTY_MODE_IO :: TTYMode
pattern TTY_MODE_IO = TTYMode #{const UV_TTY_MODE_IO}

foreign import ccall unsafe uv_tty_init :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_tty_set_mode :: Ptr UVHandle -> TTYMode -> IO CInt
foreign import ccall unsafe uv_tty_get_winsize :: Ptr UVHandle -> MBA## CInt -> MBA## CInt -> IO CInt

--------------------------------------------------------------------------------
-- fs

newtype FileMode = FileMode CInt
    deriving (Eq, Ord, Show, FiniteBits, Bits, Storable, Num)

-- | 00700 user (file owner) has read, write and execute permission
pattern S_IRWXU :: FileMode
pattern S_IRWXU = FileMode #{const S_IRWXU}

-- | 00400 user has read permission
pattern S_IRUSR :: FileMode
pattern S_IRUSR = FileMode #{const S_IRUSR}

-- | 00200 user has write permission
pattern S_IWUSR :: FileMode
pattern S_IWUSR = FileMode #{const S_IWUSR}

-- | 00100 user has execute permission
pattern S_IXUSR :: FileMode
pattern S_IXUSR = FileMode #{const S_IXUSR}

-- | 00070 group has read, write and execute permission
pattern S_IRWXG :: FileMode
pattern S_IRWXG = FileMode #{const S_IRWXG}

-- | 00040 group has read permission
pattern S_IRGRP :: FileMode
pattern S_IRGRP = FileMode #{const S_IRGRP}

-- | 00020 group has write permission
pattern S_IWGRP :: FileMode
pattern S_IWGRP = FileMode #{const S_IWGRP}

-- | 00010 group has execute permission
pattern S_IXGRP :: FileMode
pattern S_IXGRP = FileMode #{const S_IXGRP}

-- | 00007 others have read, write and execute permission
pattern S_IRWXO :: FileMode
pattern S_IRWXO = FileMode #{const S_IRWXO}

-- | 00004 others have read permission
pattern S_IROTH :: FileMode
pattern S_IROTH = FileMode #{const S_IROTH}

-- | 00002 others have write permission
pattern S_IWOTH :: FileMode
pattern S_IWOTH = FileMode #{const S_IWOTH}

-- | 00001 others have execute permission
pattern S_IXOTH :: FileMode
pattern S_IXOTH = FileMode #{const S_IXOTH}

-- | Default mode for open, 0o666(readable and writable).
pattern DEFAULT_MODE :: FileMode
pattern DEFAULT_MODE = FileMode 0o666

-- non-threaded functions
foreign import ccall unsafe hs_uv_fs_open    :: CString -> FileFlag -> FileMode -> IO UVFD
foreign import ccall unsafe hs_uv_fs_close   :: UVFD -> IO Int
foreign import ccall unsafe hs_uv_fs_read    :: UVFD -> Ptr Word8 -> Int -> Int64 -> IO Int
foreign import ccall unsafe hs_uv_fs_write   :: UVFD -> Ptr Word8 -> Int -> Int64 -> IO Int
foreign import ccall unsafe hs_uv_fs_unlink  :: CString -> IO Int
foreign import ccall unsafe hs_uv_fs_mkdir   :: CString -> FileMode -> IO Int
foreign import ccall unsafe hs_uv_fs_rmdir   :: CString -> IO Int
foreign import ccall unsafe hs_uv_fs_mkdtemp :: CString -> Int -> CString -> IO Int

-- threaded functions
foreign import ccall unsafe hs_uv_fs_open_threaded 
    :: CString -> FileFlag -> FileMode -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_close_threaded 
    :: UVFD -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_read_threaded  
    :: UVFD -> Ptr Word8 -> Int -> Int64 -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_write_threaded 
    :: UVFD -> Ptr Word8 -> Int -> Int64 -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_unlink_threaded
    :: CString -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_mkdir_threaded 
    :: CString -> FileMode -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_rmdir_threaded 
    :: CString -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_mkdtemp_threaded 
    :: CString -> Int -> CString -> Ptr UVLoop -> IO UVSlotUnsafe

newtype FileFlag = FileFlag CInt
    deriving (Eq, Ord, Show, FiniteBits, Bits, Storable, Num)

-- | The file is opened in append mode. Before each write, the file offset is positioned at the end of the file.
pattern O_APPEND :: FileFlag
pattern O_APPEND = FileFlag #{const UV_FS_O_APPEND}

-- | The file is created if it does not already exist.
pattern O_CREAT :: FileFlag
pattern O_CREAT = FileFlag #{const UV_FS_O_CREAT}

-- | File IO is done directly to and from user-space buffers, which must be aligned. Buffer size and address should be a multiple of the physical sector size of the block device, (DO NOT USE WITH Z-IO's @BufferedIO@)
pattern O_DIRECT :: FileFlag
pattern O_DIRECT = FileFlag #{const UV_FS_O_DIRECT}

-- | If the path is not a directory, fail the open. (Not useful on regular file)
--
-- Note 'o_DIRECTORY' is not supported on Windows.
pattern O_DIRECTORY :: FileFlag
pattern O_DIRECTORY = FileFlag #{const UV_FS_O_DIRECTORY}

-- |The file is opened for synchronous IO. Write operations will complete once all data and a minimum of metadata are flushed to disk.
--
-- Note 'o_DSYNC' is supported on Windows via @FILE_FLAG_WRITE_THROUGH@.
pattern O_DSYNC :: FileFlag
pattern O_DSYNC = FileFlag #{const UV_FS_O_DSYNC}

-- | If the 'o_CREAT' flag is set and the file already exists, fail the open.
--
-- Note In general, the behavior of 'o_EXCL' is undefined if it is used without 'o_CREAT'. There is one exception: on 
-- Linux 2.6 and later, 'o_EXCL' can be used without 'o_CREAT' if pathname refers to a block device. If the block 
-- device is in use by the system (e.g., mounted), the open will fail with the error @EBUSY@.
pattern O_EXCL :: FileFlag
pattern O_EXCL = FileFlag #{const UV_FS_O_EXCL}

-- | Atomically obtain an exclusive lock.
--
-- Note UV_FS_O_EXLOCK is only supported on macOS and Windows.
-- (libuv: Changed in version 1.17.0: support is added for Windows.)
pattern O_EXLOCK :: FileFlag
pattern O_EXLOCK = FileFlag #{const UV_FS_O_EXLOCK}

-- | Do not update the file access time when the file is read.
-- 
-- Note 'o_NOATIME' is not supported on Windows.
pattern O_NOATIME :: FileFlag
pattern O_NOATIME = FileFlag #{const UV_FS_O_NOATIME}

-- | If the path identifies a terminal device, opening the path will not cause that terminal to become the controlling terminal for the process (if the process does not already have one). (Not sure if this flag is useful)
--
-- Note 'o_NOCTTY' is not supported on Windows.
pattern O_NOCTTY :: FileFlag
pattern O_NOCTTY = FileFlag #{const UV_FS_O_NOCTTY}

-- | If the path is a symbolic link, fail the open.
--
-- Note 'o_NOFOLLOW' is not supported on Windows.
pattern O_NOFOLLOW :: FileFlag
pattern O_NOFOLLOW = FileFlag #{const UV_FS_O_NOFOLLOW}

-- | Open the file in nonblocking mode if possible. (Definitely not useful in Z-IO)
--
-- Note 'o_NONBLOCK' is not supported on Windows. (Not useful on regular file anyway)
pattern O_NONBLOCK :: FileFlag
pattern O_NONBLOCK = FileFlag #{const UV_FS_O_NONBLOCK}

-- | Access is intended to be random. The system can use this as a hint to optimize file caching.
-- 
-- Note 'o_RANDOM' is only supported on Windows via @FILE_FLAG_RANDOM_ACCESS@.
pattern O_RANDOM :: FileFlag
pattern O_RANDOM = FileFlag #{const UV_FS_O_RANDOM}

-- | Open the file for read-only access.
pattern O_RDONLY :: FileFlag
pattern O_RDONLY = FileFlag #{const UV_FS_O_RDONLY}

-- | Open the file for read-write access.
pattern O_RDWR :: FileFlag
pattern O_RDWR = FileFlag #{const UV_FS_O_RDWR}


-- | Access is intended to be sequential from beginning to end. The system can use this as a hint to optimize file caching.
-- 
-- Note 'o_SEQUENTIAL' is only supported on Windows via @FILE_FLAG_SEQUENTIAL_SCAN@.
pattern O_SEQUENTIAL :: FileFlag
pattern O_SEQUENTIAL = FileFlag #{const UV_FS_O_SEQUENTIAL}

-- | The file is temporary and should not be flushed to disk if possible.
--
-- Note 'o_SHORT_LIVED' is only supported on Windows via @FILE_ATTRIBUTE_TEMPORARY@.
pattern O_SHORT_LIVED :: FileFlag
pattern O_SHORT_LIVED = FileFlag #{const UV_FS_O_SHORT_LIVED}

-- | Open the symbolic link itself rather than the resource it points to.
pattern O_SYMLINK :: FileFlag
pattern O_SYMLINK = FileFlag #{const UV_FS_O_SYMLINK}

-- | The file is opened for synchronous IO. Write operations will complete once all data and all metadata are flushed to disk.
--
-- Note 'o_SYNC' is supported on Windows via @FILE_FLAG_WRITE_THROUGH@.
pattern O_SYNC :: FileFlag
pattern O_SYNC = FileFlag #{const UV_FS_O_SYNC}

-- | The file is temporary and should not be flushed to disk if possible.
--
-- Note 'o_TEMPORARY' is only supported on Windows via @FILE_ATTRIBUTE_TEMPORARY@.
pattern O_TEMPORARY :: FileFlag
pattern O_TEMPORARY = FileFlag #{const UV_FS_O_TEMPORARY}

-- | If the file exists and is a regular file, and the file is opened successfully for write access, its length shall be truncated to zero.
pattern O_TRUNC :: FileFlag
pattern O_TRUNC = FileFlag #{const UV_FS_O_TRUNC}

-- | Open the file for write-only access.
pattern O_WRONLY :: FileFlag
pattern O_WRONLY = FileFlag #{const UV_FS_O_WRONLY}

#if defined(_WIN32)
newtype UVDirEntType = UVDirEntType CInt
#else
newtype UVDirEntType = UVDirEntType CChar
#endif
    deriving (Eq, Ord, Show, FiniteBits, Bits, Storable, Num)

data DirEntType
    = DirEntUnknown
    | DirEntFile
    | DirEntDir
    | DirEntLink
    | DirEntFIFO
    | DirEntSocket
    | DirEntChar
    | DirEntBlock
  deriving (Read, Show, Eq, Ord, Generic)

fromUVDirEntType :: UVDirEntType -> DirEntType
fromUVDirEntType t
    | t == uV__DT_FILE   = DirEntFile
    | t == uV__DT_DIR    = DirEntDir
    | t == uV__DT_LINK   = DirEntLink
    | t == uV__DT_FIFO   = DirEntFIFO
    | t == uV__DT_SOCKET = DirEntSocket
    | t == uV__DT_CHAR   = DirEntChar
    | t == uV__DT_BLOCK  = DirEntBlock
    | otherwise          = DirEntUnknown

#{enum UVDirEntType, UVDirEntType,
    uV__DT_FILE    = UV__DT_FILE,
    uV__DT_DIR     = UV__DT_DIR,
    uV__DT_LINK    = UV__DT_LINK,
    uV__DT_FIFO    = UV__DT_FIFO,
    uV__DT_SOCKET  = UV__DT_SOCKET,
    uV__DT_CHAR    = UV__DT_CHAR,
    uV__DT_BLOCK   = UV__DT_BLOCK}

data UVDirEnt

peekUVDirEnt :: Ptr UVDirEnt -> IO (CString, UVDirEntType)
#ifdef HAVE_DIRENT_TYPES
peekUVDirEnt p = (,) (#{ptr hs_uv__dirent_t, d_name } p) <$> (#{peek hs_uv__dirent_t, d_type } p)
#else
peekUVDirEnt p = return ((#{ptr hs_uv__dirent_t,  d_name } p), #{const DT_UNKNOWN})
#endif

foreign import ccall unsafe hs_uv_fs_scandir_cleanup
    :: Ptr (Ptr UVDirEnt) -> Int -> IO ()
foreign import ccall unsafe hs_uv_fs_scandir
    :: CString -> MBA## (Ptr UVDirEnt) -> IO Int
foreign import ccall unsafe hs_uv_fs_scandir_extra_cleanup 
    :: Ptr (Ptr (Ptr UVDirEnt)) -> Int -> IO ()
foreign import ccall unsafe hs_uv_fs_scandir_threaded
    :: CString -> Ptr (Ptr (Ptr UVDirEnt)) -> Ptr UVLoop -> IO UVSlotUnsafe

data UVTimeSpec = UVTimeSpec 
    { uvtSecond     :: {-# UNPACK #-} !CLong
    , uvtNanoSecond :: {-# UNPACK #-} !CLong
    } deriving (Show, Read, Eq, Ord, Generic)

instance Storable UVTimeSpec where
    sizeOf _  = #{size uv_timespec_t}
    alignment _ = #{alignment uv_timespec_t}
    peek p = UVTimeSpec <$> (#{peek uv_timespec_t, tv_sec } p)
                        <*> (#{peek uv_timespec_t, tv_nsec } p)
    poke p (UVTimeSpec sec nsec) = do
        (#{poke uv_timespec_t, tv_sec  } p sec)
        (#{poke uv_timespec_t, tv_nsec } p nsec)

data FStat = FStat
    { stDev      :: {-# UNPACK #-} !Word64
    , stMode     :: {-# UNPACK #-} !Word64
    , stNlink    :: {-# UNPACK #-} !Word64
    , stUid      :: {-# UNPACK #-} !Word64
    , stGid      :: {-# UNPACK #-} !Word64
    , stRdev     :: {-# UNPACK #-} !Word64
    , stIno      :: {-# UNPACK #-} !Word64
    , stSize     :: {-# UNPACK #-} !Word64
    , stBlksize  :: {-# UNPACK #-} !Word64
    , stBlocks   :: {-# UNPACK #-} !Word64
    , stFlags    :: {-# UNPACK #-} !Word64
    , stGen      :: {-# UNPACK #-} !Word64
    , stAtim     :: {-# UNPACK #-} !UVTimeSpec
    , stMtim     :: {-# UNPACK #-} !UVTimeSpec
    , stCtim     :: {-# UNPACK #-} !UVTimeSpec
    , stBirthtim :: {-# UNPACK #-} !UVTimeSpec
    } deriving (Show, Read, Eq, Ord, Generic)

uvStatSize :: Int
uvStatSize = #{size uv_stat_t}

peekUVStat :: Ptr FStat -> IO FStat
peekUVStat p = FStat
    <$> (#{peek uv_stat_t, st_dev          } p)
    <*> (#{peek uv_stat_t, st_mode         } p)
    <*> (#{peek uv_stat_t, st_nlink        } p)
    <*> (#{peek uv_stat_t, st_uid          } p)
    <*> (#{peek uv_stat_t, st_gid          } p)
    <*> (#{peek uv_stat_t, st_rdev         } p)
    <*> (#{peek uv_stat_t, st_ino          } p)
    <*> (#{peek uv_stat_t, st_size         } p)
    <*> (#{peek uv_stat_t, st_blksize      } p)
    <*> (#{peek uv_stat_t, st_blocks       } p)
    <*> (#{peek uv_stat_t, st_flags        } p)
    <*> (#{peek uv_stat_t, st_gen          } p)
    <*> (#{peek uv_stat_t, st_atim         } p)
    <*> (#{peek uv_stat_t, st_mtim         } p)
    <*> (#{peek uv_stat_t, st_ctim         } p)
    <*> (#{peek uv_stat_t, st_birthtim     } p)

foreign import ccall unsafe hs_uv_fs_stat :: CString -> Ptr FStat -> IO Int
foreign import ccall unsafe hs_uv_fs_fstat :: UVFD -> Ptr FStat -> IO Int
foreign import ccall unsafe hs_uv_fs_lstat :: CString -> Ptr FStat -> IO Int
foreign import ccall unsafe hs_uv_fs_rename :: CString -> CString -> IO Int
foreign import ccall unsafe hs_uv_fs_fsync :: UVFD -> IO Int
foreign import ccall unsafe hs_uv_fs_fdatasync :: UVFD -> IO Int
foreign import ccall unsafe hs_uv_fs_ftruncate :: UVFD -> Int64 -> IO Int

foreign import ccall unsafe hs_uv_fs_stat_threaded
    :: CString -> Ptr FStat -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_fstat_threaded
    :: UVFD -> Ptr FStat -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_lstat_threaded
    :: CString -> Ptr FStat -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_rename_threaded
    :: CString -> CString -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_fsync_threaded
    :: UVFD -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_fdatasync_threaded
    :: UVFD -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_ftruncate_threaded 
    :: UVFD -> Int64 -> Ptr UVLoop -> IO UVSlotUnsafe

-- | Flags control copying.
-- 
--   * 'COPYFILE_EXCL': If present, uv_fs_copyfile() will fail with UV_EEXIST if the destination path already exists. The default behavior is to overwrite the destination if it exists.
--   * 'COPYFILE_FICLONE': If present, uv_fs_copyfile() will attempt to create a copy-on-write reflink. If the underlying platform does not support copy-on-write, then a fallback copy mechanism is used.
-- 
newtype CopyFileFlag = CopyFileFlag CInt
    deriving (Eq, Ord, Show, FiniteBits, Bits, Storable, Num)

pattern COPYFILE_DEFAULT :: CopyFileFlag
pattern COPYFILE_DEFAULT = CopyFileFlag 0

pattern COPYFILE_EXCL :: CopyFileFlag
pattern COPYFILE_EXCL = CopyFileFlag #{const UV_FS_COPYFILE_EXCL}

pattern COPYFILE_FICLONE :: CopyFileFlag
#ifdef UV_FS_COPYFILE_FICLONE
pattern COPYFILE_FICLONE = CopyFileFlag #{const UV_FS_COPYFILE_FICLONE}
#else
pattern COPYFILE_FICLONE = CopyFileFlag 0   -- fallback to normal copy.
#endif

foreign import ccall unsafe hs_uv_fs_copyfile :: CString -> CString -> CopyFileFlag -> IO Int
foreign import ccall unsafe hs_uv_fs_copyfile_threaded
    :: CString -> CString -> CopyFileFlag -> Ptr UVLoop -> IO UVSlotUnsafe

newtype AccessMode = AccessMode CInt
    deriving (Eq, Ord, Show, FiniteBits, Bits, Storable, Num)

pattern F_OK :: AccessMode
pattern F_OK = AccessMode #{const F_OK}
pattern R_OK :: AccessMode
pattern R_OK = AccessMode #{const R_OK}
pattern W_OK :: AccessMode
pattern W_OK = AccessMode #{const W_OK}
pattern X_OK :: AccessMode
pattern X_OK = AccessMode #{const X_OK}

data AccessResult = NoExistence | NoPermission | AccessOK deriving (Show, Eq, Ord)

foreign import ccall unsafe hs_uv_fs_access :: CString -> AccessMode -> IO Int
foreign import ccall unsafe hs_uv_fs_access_threaded
    :: CString -> AccessMode -> Ptr UVLoop -> IO UVSlotUnsafe

foreign import ccall unsafe hs_uv_fs_chmod :: CString -> FileMode -> IO Int
foreign import ccall unsafe hs_uv_fs_chmod_threaded
    :: CString -> FileMode -> Ptr UVLoop -> IO UVSlotUnsafe

foreign import ccall unsafe hs_uv_fs_fchmod :: UVFD -> FileMode -> IO Int
foreign import ccall unsafe hs_uv_fs_fchmod_threaded
    :: UVFD -> FileMode -> Ptr UVLoop -> IO UVSlotUnsafe

foreign import ccall unsafe hs_uv_fs_utime :: CString -> Double -> Double -> IO Int
foreign import ccall unsafe hs_uv_fs_utime_threaded
    :: CString -> Double -> Double -> Ptr UVLoop -> IO UVSlotUnsafe

foreign import ccall unsafe hs_uv_fs_futime :: UVFD -> Double -> Double -> IO Int
foreign import ccall unsafe hs_uv_fs_futime_threaded
    :: UVFD -> Double -> Double -> Ptr UVLoop -> IO UVSlotUnsafe

foreign import ccall unsafe hs_uv_fs_lutime :: CString -> Double -> Double -> IO Int
foreign import ccall unsafe hs_uv_fs_lutime_threaded
    :: CString -> Double -> Double -> Ptr UVLoop -> IO UVSlotUnsafe

newtype SymlinkFlag = SymlinkFlag CInt
    deriving (Eq, Ord, Show, FiniteBits, Bits, Storable, Num)

pattern SYMLINK_DEFAULT :: SymlinkFlag
pattern SYMLINK_DEFAULT = SymlinkFlag 0

pattern SYMLINK_DIR :: SymlinkFlag
pattern SYMLINK_DIR = SymlinkFlag #{const UV_FS_SYMLINK_DIR}

pattern SYMLINK_JUNCTION :: SymlinkFlag
pattern SYMLINK_JUNCTION = SymlinkFlag #{const UV_FS_SYMLINK_JUNCTION}

foreign import ccall unsafe hs_uv_fs_link :: CString -> CString -> IO Int
foreign import ccall unsafe hs_uv_fs_link_threaded
    :: CString -> CString -> Ptr UVLoop -> IO UVSlotUnsafe

foreign import ccall unsafe hs_uv_fs_symlink :: CString -> CString -> SymlinkFlag -> IO Int
foreign import ccall unsafe hs_uv_fs_symlink_threaded
    :: CString -> CString -> SymlinkFlag -> Ptr UVLoop -> IO UVSlotUnsafe

-- readlink and realpath share the same cleanup and callback
foreign import ccall unsafe hs_uv_fs_readlink_cleanup
    :: CString -> IO ()
foreign import ccall unsafe hs_uv_fs_readlink
    :: CString -> MBA## CString -> IO Int
foreign import ccall unsafe hs_uv_fs_realpath
    :: CString -> MBA## CString -> IO Int
foreign import ccall unsafe hs_uv_fs_readlink_extra_cleanup 
    :: Ptr CString -> IO ()
foreign import ccall unsafe hs_uv_fs_readlink_threaded
    :: CString -> Ptr CString -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_realpath_threaded
    :: CString -> Ptr CString -> Ptr UVLoop -> IO UVSlotUnsafe

--------------------------------------------------------------------------------
-- misc

newtype UVHandleType = UVHandleType CInt deriving (Eq, Ord, Show, Storable)

pattern UV_UNKNOWN_HANDLE :: UVHandleType
pattern UV_UNKNOWN_HANDLE = UVHandleType #{const UV_UNKNOWN_HANDLE}
pattern UV_ASYNC :: UVHandleType
pattern UV_ASYNC = UVHandleType #{const UV_ASYNC}
pattern UV_CHECK :: UVHandleType
pattern UV_CHECK = UVHandleType #{const UV_CHECK}
pattern UV_FS_EVENT :: UVHandleType
pattern UV_FS_EVENT = UVHandleType #{const UV_FS_EVENT}
pattern UV_FS_POLL :: UVHandleType
pattern UV_FS_POLL = UVHandleType #{const UV_FS_POLL}
pattern UV_HANDLE :: UVHandleType
pattern UV_HANDLE = UVHandleType #{const UV_HANDLE}
pattern UV_IDLE :: UVHandleType
pattern UV_IDLE = UVHandleType #{const UV_IDLE}
pattern UV_NAMED_PIPE :: UVHandleType
pattern UV_NAMED_PIPE = UVHandleType #{const UV_NAMED_PIPE}
pattern UV_POLL :: UVHandleType
pattern UV_POLL = UVHandleType #{const UV_POLL}
pattern UV_PREPARE :: UVHandleType
pattern UV_PREPARE = UVHandleType #{const UV_PREPARE}
pattern UV_PROCESS :: UVHandleType
pattern UV_PROCESS = UVHandleType #{const UV_PROCESS}
pattern UV_STREAM :: UVHandleType
pattern UV_STREAM = UVHandleType #{const UV_STREAM}
pattern UV_TCP :: UVHandleType
pattern UV_TCP = UVHandleType #{const UV_TCP}
pattern UV_TIMER :: UVHandleType
pattern UV_TIMER = UVHandleType #{const UV_TIMER}
pattern UV_TTY :: UVHandleType
pattern UV_TTY = UVHandleType #{const UV_TTY}
pattern UV_UDP :: UVHandleType
pattern UV_UDP = UVHandleType #{const UV_UDP}
pattern UV_SIGNAL :: UVHandleType
pattern UV_SIGNAL = UVHandleType #{const UV_SIGNAL}
pattern UV_FILE :: UVHandleType
pattern UV_FILE = UVHandleType #{const UV_FILE}

foreign import ccall unsafe uv_guess_handle :: UVFD -> IO UVHandleType

foreign import ccall unsafe uv_resident_set_memory :: MBA## CSize -> IO CInt
foreign import ccall unsafe uv_uptime :: MBA## Double -> IO CInt
foreign import ccall unsafe uv_getrusage :: MBA## a -> IO CInt

-- | Data type for storing times.
-- typedef struct { long tv_sec; long tv_usec; } uv_timeval_t;
data TimeVal = TimeVal 
    { tv_sec  :: {-# UNPACK #-} !CLong
    , tv_usec :: {-# UNPACK #-} !CLong
    } deriving (Show, Read, Eq, Ord)

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
    } deriving (Show, Read, Eq, Ord)

sizeOfResUsage :: Int
sizeOfResUsage = #size uv_rusage_t

peekResUsage :: MBA## a -> IO ResUsage
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

newtype PID = PID CInt deriving (Eq, Ord, Show, Storable)

pattern PRIORITY_LOW          :: CInt
pattern PRIORITY_BELOW_NORMAL :: CInt
pattern PRIORITY_NORMAL       :: CInt
pattern PRIORITY_ABOVE_NORMAL :: CInt
pattern PRIORITY_HIGH         :: CInt
pattern PRIORITY_HIGHEST      :: CInt
pattern PRIORITY_LOW           = (#const UV_PRIORITY_LOW           )
pattern PRIORITY_BELOW_NORMAL  = (#const UV_PRIORITY_BELOW_NORMAL  )
pattern PRIORITY_NORMAL        = (#const UV_PRIORITY_NORMAL        )
pattern PRIORITY_ABOVE_NORMAL  = (#const UV_PRIORITY_ABOVE_NORMAL  )
pattern PRIORITY_HIGH          = (#const UV_PRIORITY_HIGH          )
pattern PRIORITY_HIGHEST       = (#const UV_PRIORITY_HIGHEST       )

foreign import ccall unsafe uv_hrtime :: IO Word64

foreign import ccall unsafe uv_os_environ :: MBA## (Ptr a) -> MBA## CInt -> IO CInt
foreign import ccall unsafe uv_os_free_environ :: Ptr a -> CInt -> IO ()
foreign import ccall unsafe uv_os_getenv :: CString -> CString -> MBA## CSize -> IO CInt
foreign import ccall unsafe uv_os_setenv :: CString -> CString -> IO CInt
foreign import ccall unsafe uv_os_unsetenv :: CString -> IO CInt

pattern UV_MAXHOSTNAMESIZE :: CSize
pattern UV_MAXHOSTNAMESIZE = #const UV_MAXHOSTNAMESIZE
foreign import ccall unsafe uv_os_gethostname :: CString -> MBA## CSize -> IO CInt

data OSName = OSName
    { os_sysname :: T.Text
    , os_release :: T.Text
    , os_version :: T.Text
    , os_machine :: T.Text
    } deriving (Eq, Ord, Show, Read)

getOSName :: IO OSName
getOSName = do
    mpa@(A.MutablePrimArray mba##) <- A.newArr (#size uv_utsname_t)
    throwUVIfMinus_ (uv_os_uname mba##)
    pa <- A.unsafeFreezeArr mpa
    let v  = V.PrimVector pa 0 (#size uv_utsname_t)
        sn = T.validate . V.takeWhile (/= 0) $ (V.drop (#offset uv_utsname_t, sysname)) v
        re = T.validate . V.takeWhile (/= 0) $ (V.drop (#offset uv_utsname_t, release)) v
        ve = T.validate . V.takeWhile (/= 0) $ (V.drop (#offset uv_utsname_t, version)) v
        ma = T.validate . V.takeWhile (/= 0) $ (V.drop (#offset uv_utsname_t, machine)) v
    return (OSName sn re ve ma)
    
foreign import ccall unsafe uv_os_uname :: MBA## OSName -> IO CInt

foreign import ccall unsafe hs_uv_random :: MBA## Word8 -> CSize -> CInt -> IO CInt
foreign import ccall unsafe hs_uv_random_threaded :: Ptr Word8 -> CSize -> CInt -> Ptr UVLoop -> IO UVSlotUnsafe
