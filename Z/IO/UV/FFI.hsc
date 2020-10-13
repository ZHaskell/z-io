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
import           Data.Primitive.Types   (Prim)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.Storable
import           Z.Data.Array.Unaligned
import qualified Z.Data.Array  as A 
import qualified Z.Data.Text   as T
import           Z.Data.Text.ShowT   (ShowT(..))
import           Z.Data.JSON         (EncodeJSON, ToValue, FromValue)
import           Z.Data.CBytes as CBytes
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
type UVFD = CInt

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

type UVRunMode = CInt 

pattern UV_RUN_DEFAULT :: UVRunMode
pattern UV_RUN_DEFAULT = #const UV_RUN_DEFAULT
pattern UV_RUN_ONCE :: UVRunMode
pattern UV_RUN_ONCE    = #const UV_RUN_ONCE
pattern UV_RUN_NOWAIT :: UVRunMode
pattern UV_RUN_NOWAIT  = #const UV_RUN_NOWAIT

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
foreign import ccall unsafe uv_unref :: Ptr UVHandle -> IO ()

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

foreign import ccall unsafe hs_uv_accept_check_alloc :: IO (Ptr UVHandle)
foreign import ccall unsafe hs_uv_accept_check_init :: Ptr UVHandle -- ^ uv_check_t
                                                    -> Ptr UVHandle -- ^ uv_stream_t
                                                    -> IO CInt
foreign import ccall unsafe hs_uv_accept_check_start :: Ptr UVHandle -> IO CInt
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
uV_TCP_IPV6ONLY = #const UV_TCP_IPV6ONLY

foreign import ccall unsafe uv_tcp_bind :: Ptr UVHandle -> MBA## SocketAddr -> CUInt -> IO CInt
foreign import ccall unsafe hs_uv_tcp_connect :: Ptr UVHandle -> MBA## SocketAddr -> IO UVSlotUnsafe
foreign import ccall unsafe hs_set_socket_reuse :: Ptr UVHandle -> IO CInt

foreign import ccall unsafe hs_uv_pipe_open :: Ptr UVHandle -> UVFD -> IO CInt
foreign import ccall unsafe uv_pipe_init :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_pipe_bind :: Ptr UVHandle -> BA## Word8 -> IO CInt
foreign import ccall unsafe hs_uv_pipe_connect :: Ptr UVHandle -> BA## Word8 -> IO UVSlotUnsafe

--------------------------------------------------------------------------------
-- udp
foreign import ccall unsafe uv_udp_init :: Ptr UVLoop -> Ptr UVHandle -> IO CInt
foreign import ccall unsafe uv_udp_init_ex :: Ptr UVLoop -> Ptr UVHandle -> CUInt -> IO CInt
foreign import ccall unsafe uv_udp_open :: Ptr UVHandle -> UVFD -> IO CInt
foreign import ccall unsafe uv_udp_bind :: Ptr UVHandle -> MBA## SocketAddr -> UDPFlag -> IO CInt

type Membership = CInt 

pattern LEAVE_GROUP :: Membership
pattern LEAVE_GROUP = #const UV_LEAVE_GROUP
pattern JOIN_GROUP :: Membership
pattern JOIN_GROUP = #const UV_JOIN_GROUP

type UDPFlag = CInt 

pattern UDP_DEFAULT        :: UDPFlag
pattern UDP_DEFAULT         = 0
pattern UDP_IPV6ONLY       :: UDPFlag
pattern UDP_IPV6ONLY        = #const UV_UDP_IPV6ONLY
pattern UDP_REUSEADDR      :: UDPFlag
pattern UDP_REUSEADDR       = #const UV_UDP_REUSEADDR

pattern UV_UDP_PARTIAL     :: Int32
pattern UV_UDP_PARTIAL      = #const UV_UDP_PARTIAL

foreign import ccall unsafe uv_udp_connect
    :: Ptr UVHandle -> MBA## SocketAddr -> IO CInt
-- | Just pass null pointer as SocketAddr to disconnect
foreign import ccall unsafe "uv_udp_connect" uv_udp_disconnect
    :: Ptr UVHandle -> Ptr SocketAddr -> IO CInt

foreign import ccall unsafe uv_udp_set_membership ::
    Ptr UVHandle -> BA## Word8 -> BA## Word8 -> Membership -> IO CInt
foreign import ccall unsafe uv_udp_set_source_membership ::
    Ptr UVHandle -> BA## Word8 -> BA## Word8 -> BA## Word8 -> Membership -> IO CInt

foreign import ccall unsafe uv_udp_set_multicast_loop :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_udp_set_multicast_ttl :: Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_udp_set_multicast_interface :: Ptr UVHandle -> BA## Word8 -> IO CInt
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
type TTYMode = CInt

pattern TTY_MODE_NORMAL :: TTYMode
pattern TTY_MODE_NORMAL = #const UV_TTY_MODE_NORMAL
pattern TTY_MODE_RAW :: TTYMode
pattern TTY_MODE_RAW = #const UV_TTY_MODE_RAW
pattern TTY_MODE_IO :: TTYMode
pattern TTY_MODE_IO = #const UV_TTY_MODE_IO

foreign import ccall unsafe uv_tty_init :: Ptr UVLoop -> Ptr UVHandle -> CInt -> IO CInt
foreign import ccall unsafe uv_tty_set_mode :: Ptr UVHandle -> TTYMode -> IO CInt
foreign import ccall unsafe uv_tty_get_winsize :: Ptr UVHandle -> MBA## CInt -> MBA## CInt -> IO CInt

--------------------------------------------------------------------------------
-- fs

type FileMode = CInt

-- | 00700 user (file owner) has read, write and execute permission
pattern S_IRWXU :: FileMode
pattern S_IRWXU = #const S_IRWXU

-- | 00400 user has read permission
pattern S_IRUSR :: FileMode
pattern S_IRUSR = #const S_IRUSR

-- | 00200 user has write permission
pattern S_IWUSR :: FileMode
pattern S_IWUSR = #const S_IWUSR

-- | 00100 user has execute permission
pattern S_IXUSR :: FileMode
pattern S_IXUSR = #const S_IXUSR

-- | 00070 group has read, write and execute permission
pattern S_IRWXG :: FileMode
pattern S_IRWXG = #const S_IRWXG

-- | 00040 group has read permission
pattern S_IRGRP :: FileMode
pattern S_IRGRP = #const S_IRGRP

-- | 00020 group has write permission
pattern S_IWGRP :: FileMode
pattern S_IWGRP = #const S_IWGRP

-- | 00010 group has execute permission
pattern S_IXGRP :: FileMode
pattern S_IXGRP = #const S_IXGRP

-- | 00007 others have read, write and execute permission
pattern S_IRWXO :: FileMode
pattern S_IRWXO = #const S_IRWXO

-- | 00004 others have read permission
pattern S_IROTH :: FileMode
pattern S_IROTH = #const S_IROTH

-- | 00002 others have write permission
pattern S_IWOTH :: FileMode
pattern S_IWOTH = #const S_IWOTH

-- | 00001 others have execute permission
pattern S_IXOTH :: FileMode
pattern S_IXOTH = #const S_IXOTH

-- | Default mode for open, 0o666(readable and writable).
pattern DEFAULT_MODE :: FileMode
pattern DEFAULT_MODE = 0o666

-- non-threaded functions
foreign import ccall unsafe hs_uv_fs_open    :: BA## Word8 -> FileFlag -> FileMode -> IO UVFD
foreign import ccall unsafe hs_uv_fs_close   :: UVFD -> IO Int
foreign import ccall unsafe hs_uv_fs_read    :: UVFD -> Ptr Word8 -> Int -> Int64 -> IO Int
foreign import ccall unsafe hs_uv_fs_write   :: UVFD -> Ptr Word8 -> Int -> Int64 -> IO Int
foreign import ccall unsafe hs_uv_fs_unlink  :: BA## Word8 -> IO Int
foreign import ccall unsafe hs_uv_fs_mkdir   :: BA## Word8 -> FileMode -> IO Int
foreign import ccall unsafe hs_uv_fs_rmdir   :: BA## Word8 -> IO Int
foreign import ccall unsafe hs_uv_fs_mkdtemp :: BA## Word8 -> Int -> MBA## Word8 -> IO Int

-- threaded functions
foreign import ccall unsafe hs_uv_fs_open_threaded 
    :: BA## Word8 -> FileFlag -> FileMode -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_close_threaded 
    :: UVFD -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_read_threaded  
    :: UVFD -> Ptr Word8 -> Int -> Int64 -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_write_threaded 
    :: UVFD -> Ptr Word8 -> Int -> Int64 -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_unlink_threaded
    :: BA## Word8 -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_mkdir_threaded 
    :: BA## Word8 -> FileMode -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_rmdir_threaded 
    :: BA## Word8 -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_mkdtemp_threaded 
    :: BA## Word8 -> Int -> MBA## Word8 -> Ptr UVLoop -> IO UVSlotUnsafe

type FileFlag = CInt

-- | The file is opened in append mode. Before each write, the file offset is positioned at the end of the file.
pattern O_APPEND :: FileFlag
pattern O_APPEND = #const UV_FS_O_APPEND

-- | The file is created if it does not already exist.
pattern O_CREAT :: FileFlag
pattern O_CREAT = #const UV_FS_O_CREAT

-- | File IO is done directly to and from user-space buffers, which must be aligned. Buffer size and address should be a multiple of the physical sector size of the block device, (DO NOT USE WITH Z-IO's @BufferedIO@)
pattern O_DIRECT :: FileFlag
pattern O_DIRECT = #const UV_FS_O_DIRECT

-- | If the path is not a directory, fail the open. (Not useful on regular file)
--
-- Note 'o_DIRECTORY' is not supported on Windows.
pattern O_DIRECTORY :: FileFlag
pattern O_DIRECTORY = #const UV_FS_O_DIRECTORY

-- |The file is opened for synchronous IO. Write operations will complete once all data and a minimum of metadata are flushed to disk.
--
-- Note 'o_DSYNC' is supported on Windows via @FILE_FLAG_WRITE_THROUGH@.
pattern O_DSYNC :: FileFlag
pattern O_DSYNC = #const UV_FS_O_DSYNC

-- | If the 'o_CREAT' flag is set and the file already exists, fail the open.
--
-- Note In general, the behavior of 'o_EXCL' is undefined if it is used without 'o_CREAT'. There is one exception: on 
-- Linux 2.6 and later, 'o_EXCL' can be used without 'o_CREAT' if pathname refers to a block device. If the block 
-- device is in use by the system (e.g., mounted), the open will fail with the error @EBUSY@.
pattern O_EXCL :: FileFlag
pattern O_EXCL = #const UV_FS_O_EXCL

-- | Atomically obtain an exclusive lock.
--
-- Note UV_FS_O_EXLOCK is only supported on macOS and Windows.
-- (libuv: Changed in version 1.17.0: support is added for Windows.)
pattern O_EXLOCK :: FileFlag
pattern O_EXLOCK = #const UV_FS_O_EXLOCK

-- | Do not update the file access time when the file is read.
-- 
-- Note 'o_NOATIME' is not supported on Windows.
pattern O_NOATIME :: FileFlag
pattern O_NOATIME = #const UV_FS_O_NOATIME

-- | If the path identifies a terminal device, opening the path will not cause that terminal to become the controlling terminal for the process (if the process does not already have one). (Not sure if this flag is useful)
--
-- Note 'o_NOCTTY' is not supported on Windows.
pattern O_NOCTTY :: FileFlag
pattern O_NOCTTY = #const UV_FS_O_NOCTTY

-- | If the path is a symbolic link, fail the open.
--
-- Note 'o_NOFOLLOW' is not supported on Windows.
pattern O_NOFOLLOW :: FileFlag
pattern O_NOFOLLOW = #const UV_FS_O_NOFOLLOW

-- | Open the file in nonblocking mode if possible. (Definitely not useful in Z-IO)
--
-- Note 'o_NONBLOCK' is not supported on Windows. (Not useful on regular file anyway)
pattern O_NONBLOCK :: FileFlag
pattern O_NONBLOCK = #const UV_FS_O_NONBLOCK

-- | Access is intended to be random. The system can use this as a hint to optimize file caching.
-- 
-- Note 'o_RANDOM' is only supported on Windows via @FILE_FLAG_RANDOM_ACCESS@.
pattern O_RANDOM :: FileFlag
pattern O_RANDOM = #const UV_FS_O_RANDOM

-- | Open the file for read-only access.
pattern O_RDONLY :: FileFlag
pattern O_RDONLY = #const UV_FS_O_RDONLY

-- | Open the file for read-write access.
pattern O_RDWR :: FileFlag
pattern O_RDWR = #const UV_FS_O_RDWR


-- | Access is intended to be sequential from beginning to end. The system can use this as a hint to optimize file caching.
-- 
-- Note 'o_SEQUENTIAL' is only supported on Windows via @FILE_FLAG_SEQUENTIAL_SCAN@.
pattern O_SEQUENTIAL :: FileFlag
pattern O_SEQUENTIAL = #const UV_FS_O_SEQUENTIAL

-- | The file is temporary and should not be flushed to disk if possible.
--
-- Note 'o_SHORT_LIVED' is only supported on Windows via @FILE_ATTRIBUTE_TEMPORARY@.
pattern O_SHORT_LIVED :: FileFlag
pattern O_SHORT_LIVED = #const UV_FS_O_SHORT_LIVED

-- | Open the symbolic link itself rather than the resource it points to.
pattern O_SYMLINK :: FileFlag
pattern O_SYMLINK = #const UV_FS_O_SYMLINK

-- | The file is opened for synchronous IO. Write operations will complete once all data and all metadata are flushed to disk.
--
-- Note 'o_SYNC' is supported on Windows via @FILE_FLAG_WRITE_THROUGH@.
pattern O_SYNC :: FileFlag
pattern O_SYNC = #const UV_FS_O_SYNC

-- | The file is temporary and should not be flushed to disk if possible.
--
-- Note 'o_TEMPORARY' is only supported on Windows via @FILE_ATTRIBUTE_TEMPORARY@.
pattern O_TEMPORARY :: FileFlag
pattern O_TEMPORARY = #const UV_FS_O_TEMPORARY

-- | If the file exists and is a regular file, and the file is opened successfully for write access, its length shall be truncated to zero.
pattern O_TRUNC :: FileFlag
pattern O_TRUNC = #const UV_FS_O_TRUNC

-- | Open the file for write-only access.
pattern O_WRONLY :: FileFlag
pattern O_WRONLY = #const UV_FS_O_WRONLY

#if defined(_WIN32)
type UVDirEntType = CInt
#else
type UVDirEntType = CChar
#endif

data DirEntType
    = DirEntUnknown
    | DirEntFile
    | DirEntDir
    | DirEntLink
    | DirEntFIFO
    | DirEntSocket
    | DirEntChar
    | DirEntBlock
  deriving (Read, Show, Eq, Ord, Enum, Generic)
    deriving anyclass (ShowT, EncodeJSON, ToValue, FromValue)

fromUVDirEntType :: UVDirEntType -> DirEntType
fromUVDirEntType t
    | t == #{const UV__DT_FILE  } = DirEntFile
    | t == #{const UV__DT_DIR   } = DirEntDir
    | t == #{const UV__DT_LINK  } = DirEntLink
    | t == #{const UV__DT_FIFO  } = DirEntFIFO
    | t == #{const UV__DT_SOCKET} = DirEntSocket
    | t == #{const UV__DT_CHAR  } = DirEntChar
    | t == #{const UV__DT_BLOCK } = DirEntBlock
    | otherwise          = DirEntUnknown

peekUVDirEnt :: Ptr DirEntType -> IO (CString, UVDirEntType)
#ifdef HAVE_DIRENT_TYPES
peekUVDirEnt p = (,) (#{ptr hs_uv__dirent_t, d_name } p) <$> (#{peek hs_uv__dirent_t, d_type } p)
#else
peekUVDirEnt p = return ((#{ptr hs_uv__dirent_t,  d_name } p), #{const DT_UNKNOWN})
#endif

foreign import ccall unsafe hs_uv_fs_scandir_cleanup
    :: Ptr (Ptr DirEntType) -> Int -> IO ()
foreign import ccall unsafe hs_uv_fs_scandir
    :: BA## Word8 -> MBA## (Ptr DirEntType) -> IO Int
foreign import ccall unsafe hs_uv_fs_scandir_extra_cleanup 
    :: Ptr (Ptr (Ptr DirEntType)) -> Int -> IO ()
foreign import ccall unsafe hs_uv_fs_scandir_threaded
    :: BA## Word8 -> Ptr (Ptr (Ptr DirEntType)) -> Ptr UVLoop -> IO UVSlotUnsafe

data UVTimeSpec = UVTimeSpec 
    { uvtSecond     :: {-# UNPACK #-} !CLong
    , uvtNanoSecond :: {-# UNPACK #-} !CLong
    } deriving (Show, Read, Eq, Ord, Generic)
        deriving anyclass (ShowT, EncodeJSON, ToValue, FromValue)

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
        deriving anyclass (ShowT, EncodeJSON, ToValue, FromValue)

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

foreign import ccall unsafe hs_uv_fs_stat :: BA## Word8 -> Ptr FStat -> IO Int
foreign import ccall unsafe hs_uv_fs_fstat :: UVFD -> Ptr FStat -> IO Int
foreign import ccall unsafe hs_uv_fs_lstat :: BA## Word8 -> Ptr FStat -> IO Int
foreign import ccall unsafe hs_uv_fs_rename :: BA## Word8 -> BA## Word8 -> IO Int
foreign import ccall unsafe hs_uv_fs_fsync :: UVFD -> IO Int
foreign import ccall unsafe hs_uv_fs_fdatasync :: UVFD -> IO Int
foreign import ccall unsafe hs_uv_fs_ftruncate :: UVFD -> Int64 -> IO Int

foreign import ccall unsafe hs_uv_fs_stat_threaded
    :: BA## Word8 -> Ptr FStat -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_fstat_threaded
    :: UVFD -> Ptr FStat -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_lstat_threaded
    :: BA## Word8 -> Ptr FStat -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_rename_threaded
    :: BA## Word8 -> BA## Word8 -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_fsync_threaded
    :: UVFD -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_fdatasync_threaded
    :: UVFD -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_ftruncate_threaded 
    :: UVFD -> Int64 -> Ptr UVLoop -> IO UVSlotUnsafe

-- | Flags control copying.
-- 
--  * 'COPYFILE_EXCL': If present, uv_fs_copyfile() will fail with UV_EEXIST if the destination path already exists. The default behavior is to overwrite the destination if it exists.
--  * 'COPYFILE_FICLONE': If present, uv_fs_copyfile() will attempt to create a copy-on-write reflink. If the underlying platform does not support copy-on-write, then a fallback copy mechanism is used.
--  * 'COPYFILE_FICLONE_FORCE': If present, uv_fs_copyfile() will attempt to create a copy-on-write reflink. If the underlying platform does not support copy-on-write, or an error occurs while attempting to use copy-on-write, then an error is returned.
type CopyFileFlag = CInt

pattern COPYFILE_DEFAULT :: CopyFileFlag
pattern COPYFILE_DEFAULT = 0

pattern COPYFILE_EXCL :: CopyFileFlag
pattern COPYFILE_EXCL = #const UV_FS_COPYFILE_EXCL

pattern COPYFILE_FICLONE :: CopyFileFlag
pattern COPYFILE_FICLONE = #const UV_FS_COPYFILE_FICLONE

pattern COPYFILE_FICLONE_FORCE :: CopyFileFlag
pattern COPYFILE_FICLONE_FORCE = #const UV_FS_COPYFILE_FICLONE_FORCE

foreign import ccall unsafe hs_uv_fs_copyfile :: BA## Word8 -> BA## Word8 -> CopyFileFlag -> IO Int
foreign import ccall unsafe hs_uv_fs_copyfile_threaded
    :: BA## Word8 -> BA## Word8 -> CopyFileFlag -> Ptr UVLoop -> IO UVSlotUnsafe

type AccessMode = CInt

pattern F_OK :: AccessMode
pattern F_OK = #const F_OK
pattern R_OK :: AccessMode
pattern R_OK = #const R_OK
pattern W_OK :: AccessMode
pattern W_OK = #const W_OK
pattern X_OK :: AccessMode
pattern X_OK = #const X_OK

data AccessResult = NoExistence | NoPermission | AccessOK 
    deriving (Show, Eq, Ord, Enum, Generic)
    deriving anyclass (ShowT, EncodeJSON, ToValue, FromValue)

foreign import ccall unsafe hs_uv_fs_access :: BA## Word8 -> AccessMode -> IO Int
foreign import ccall unsafe hs_uv_fs_access_threaded
    :: BA## Word8 -> AccessMode -> Ptr UVLoop -> IO UVSlotUnsafe

foreign import ccall unsafe hs_uv_fs_chmod :: BA## Word8 -> FileMode -> IO Int
foreign import ccall unsafe hs_uv_fs_chmod_threaded
    :: BA## Word8 -> FileMode -> Ptr UVLoop -> IO UVSlotUnsafe

foreign import ccall unsafe hs_uv_fs_fchmod :: UVFD -> FileMode -> IO Int
foreign import ccall unsafe hs_uv_fs_fchmod_threaded
    :: UVFD -> FileMode -> Ptr UVLoop -> IO UVSlotUnsafe

foreign import ccall unsafe hs_uv_fs_utime :: BA## Word8 -> Double -> Double -> IO Int
foreign import ccall unsafe hs_uv_fs_utime_threaded
    :: BA## Word8 -> Double -> Double -> Ptr UVLoop -> IO UVSlotUnsafe

foreign import ccall unsafe hs_uv_fs_futime :: UVFD -> Double -> Double -> IO Int
foreign import ccall unsafe hs_uv_fs_futime_threaded
    :: UVFD -> Double -> Double -> Ptr UVLoop -> IO UVSlotUnsafe

foreign import ccall unsafe hs_uv_fs_lutime :: BA## Word8 -> Double -> Double -> IO Int
foreign import ccall unsafe hs_uv_fs_lutime_threaded
    :: BA## Word8 -> Double -> Double -> Ptr UVLoop -> IO UVSlotUnsafe

-- | On Windows the flags parameter can be specified to control how the symlink will be created:
--
-- * 'SYMLINK_DIR': indicates that path points to a directory.
-- * 'SYMLINK_JUNCTION': request that the symlink is created using junction points.
type SymlinkFlag = CInt

pattern SYMLINK_DEFAULT :: SymlinkFlag
pattern SYMLINK_DEFAULT = 0

pattern SYMLINK_DIR :: SymlinkFlag
pattern SYMLINK_DIR = #const UV_FS_SYMLINK_DIR

pattern SYMLINK_JUNCTION :: SymlinkFlag
pattern SYMLINK_JUNCTION = #const UV_FS_SYMLINK_JUNCTION

foreign import ccall unsafe hs_uv_fs_link :: BA## Word8 -> BA## Word8 -> IO Int
foreign import ccall unsafe hs_uv_fs_link_threaded
    :: BA## Word8 -> BA## Word8 -> Ptr UVLoop -> IO UVSlotUnsafe

foreign import ccall unsafe hs_uv_fs_symlink :: BA## Word8 -> BA## Word8 -> SymlinkFlag -> IO Int
foreign import ccall unsafe hs_uv_fs_symlink_threaded
    :: BA## Word8 -> BA## Word8 -> SymlinkFlag -> Ptr UVLoop -> IO UVSlotUnsafe

-- readlink and realpath share the same cleanup and callback
foreign import ccall unsafe hs_uv_fs_readlink_cleanup
    :: CString -> IO ()
foreign import ccall unsafe hs_uv_fs_readlink
    :: BA## Word8 -> MBA## CString -> IO Int
foreign import ccall unsafe hs_uv_fs_realpath
    :: BA## Word8  -> MBA## CString -> IO Int
foreign import ccall unsafe hs_uv_fs_readlink_extra_cleanup 
    :: Ptr CString -> IO ()
foreign import ccall unsafe hs_uv_fs_readlink_threaded
    :: BA## Word8  -> Ptr CString -> Ptr UVLoop -> IO UVSlotUnsafe
foreign import ccall unsafe hs_uv_fs_realpath_threaded
    :: BA## Word8  -> Ptr CString -> Ptr UVLoop -> IO UVSlotUnsafe

--------------------------------------------------------------------------------
-- process

newtype UID = UID 
#if defined(_WIN32)
    Word8
#else
    Word32
#endif
   deriving (Eq, Ord, Show, Generic)
   deriving newtype (Storable, Prim, Unaligned, EncodeJSON, ToValue, FromValue)
   deriving anyclass ShowT

newtype GID = GID 
#if defined(_WIN32)
    Word8
#else
    Word32
#endif
   deriving (Eq, Ord, Show, Generic)
   deriving newtype (Storable, Prim, Unaligned, EncodeJSON, ToValue, FromValue)
   deriving anyclass ShowT

type ProcessFlag = CUInt

-- | Set the child process' user id.
--
-- This is not supported on Windows, uv_spawn() will fail and set the error to UV_ENOTSUP.
pattern PROCESS_SETUID :: ProcessFlag
pattern PROCESS_SETUID = (#const UV_PROCESS_SETUID)
-- | Set the child process' user id.
--
-- This is not supported on Windows, uv_spawn() will fail and set the error to UV_ENOTSUP.
pattern PROCESS_SETGID :: ProcessFlag
pattern PROCESS_SETGID = (#const UV_PROCESS_SETGID)
-- | Do not wrap any arguments in quotes, or perform any other escaping, when
-- converting the argument list into a command line string. 
--
-- This option is only meaningful on Windows systems. On Unix it is silently ignored.
pattern PROCESS_WINDOWS_VERBATIM_ARGUMENTS :: ProcessFlag
pattern PROCESS_WINDOWS_VERBATIM_ARGUMENTS = (#const UV_PROCESS_WINDOWS_VERBATIM_ARGUMENTS)
-- | Spawn the child process in a detached state 
--
-- This will make it a process group leader, and will effectively enable the child to keep running after
-- the parent exits.
pattern PROCESS_DETACHED :: ProcessFlag
pattern PROCESS_DETACHED = (#const UV_PROCESS_DETACHED)
-- | Hide the subprocess window that would normally be created.
--
-- This option is only meaningful on Windows systems. On Unix it is silently ignored.
pattern PROCESS_WINDOWS_HIDE :: ProcessFlag
pattern PROCESS_WINDOWS_HIDE = (#const UV_PROCESS_WINDOWS_HIDE)
-- | Hide the subprocess console window that would normally be created. 
--
-- This option is only meaningful on Windows systems. On Unix it is silently ignored.
pattern PROCESS_WINDOWS_HIDE_CONSOLE :: ProcessFlag
pattern PROCESS_WINDOWS_HIDE_CONSOLE = (#const UV_PROCESS_WINDOWS_HIDE_CONSOLE)
-- | Hide the subprocess GUI window that would normally be created. 
--
-- This option is only meaningful on Windows systems. On Unix it is silently ignored.
pattern PROCESS_WINDOWS_HIDE_GUI :: ProcessFlag
pattern PROCESS_WINDOWS_HIDE_GUI = (#const UV_PROCESS_WINDOWS_HIDE_GUI)


{- typedef struct uv_process_options_s {
    uv_exit_cb exit_cb;
    const char* file;
    char** args;
    char** env;
    const char* cwd;
    unsigned int flags;
    int stdio_count;
    uv_stdio_container_t* stdio;
    uv_uid_t uid;
    uv_gid_t gid;
} uv_process_options_t;
-}

data ProcessOptions = ProcessOptions
    { processFile :: CBytes                     -- ^ Path pointing to the program to be executed.
    , processArgs :: [CBytes]                   -- ^ Command line arguments. 
                                                -- On Windows this uses CreateProcess which concatenates
                                                -- the arguments into a string this can cause some strange errors.
                                                -- See the 'PROCESS_WINDOWS_VERBATIM_ARGUMENTS'.
    , processEnv  :: Maybe [(CBytes, CBytes)]   -- ^ Optional environment(otherwise inherit from the current process).
    , processCWD :: CBytes                      -- ^ Current working directory for the subprocess.
    , processFlags :: ProcessFlag               -- ^ Various flags that control how spawn behaves
    , processUID :: UID -- ^ This happens only when the appropriate bits are set in the flags fields.
    , processGID :: GID -- ^ This happens only when the appropriate bits are set in the flags fields.
    , processStdStreams :: (ProcessStdStream, ProcessStdStream, ProcessStdStream) -- ^ Specifying how (stdin, stdout, stderr) should be passed/created to the child, see 'ProcessStdStream'
                            
    }  deriving (Eq, Ord, Show, Generic)
       deriving anyclass ShowT

data ProcessStdStream
    = ProcessIgnore     -- ^ redirect process std stream to /dev/null
    | ProcessCreate     -- ^ create a new std stream
    | ProcessInherit UVFD -- ^ pass an existing FD to child process as std stream
  deriving  (Eq, Ord, Show, Generic)
  deriving anyclass ShowT

processStdStreamFlag :: ProcessStdStream -> CInt
processStdStreamFlag ProcessIgnore = #const UV_IGNORE
processStdStreamFlag ProcessCreate = (#const UV_CREATE_PIPE) 
                            .|. (#const UV_READABLE_PIPE)
                            .|. (#const UV_WRITABLE_PIPE)
processStdStreamFlag (ProcessInherit _) = #const UV_INHERIT_FD

foreign import ccall unsafe hs_uv_spawn :: Ptr UVLoop 
                                        -> MBA## ProcessOptions         --  option
                                        -> BA## Word8                   --  file
                                        -> BA## Word8                   --  all args
                                        -> CSize                        --  args len
                                        -> BA## Word8                   --  all envs
                                        -> CSsize                        --  envs len
                                        -> BA## Word8                   --  cwd
                                        -> MBA## ProcessStdStream       -- stdio
                                        -> IO Int

foreign import ccall unsafe uv_kill :: CInt -> CInt -> IO CInt

--------------------------------------------------------------------------------
-- misc

type UVHandleType = CInt

pattern UV_UNKNOWN_HANDLE :: UVHandleType
pattern UV_UNKNOWN_HANDLE = #{const UV_UNKNOWN_HANDLE}
pattern UV_ASYNC :: UVHandleType
pattern UV_ASYNC = #const UV_ASYNC
pattern UV_CHECK :: UVHandleType
pattern UV_CHECK = #const UV_CHECK
pattern UV_FS_EVENT :: UVHandleType
pattern UV_FS_EVENT = #const UV_FS_EVENT
pattern UV_FS_POLL :: UVHandleType
pattern UV_FS_POLL = #const UV_FS_POLL
pattern UV_HANDLE :: UVHandleType
pattern UV_HANDLE = #const UV_HANDLE
pattern UV_IDLE :: UVHandleType
pattern UV_IDLE = #const UV_IDLE
pattern UV_NAMED_PIPE :: UVHandleType
pattern UV_NAMED_PIPE = #const UV_NAMED_PIPE
pattern UV_POLL :: UVHandleType
pattern UV_POLL = #const UV_POLL
pattern UV_PREPARE :: UVHandleType
pattern UV_PREPARE = #const UV_PREPARE
pattern UV_PROCESS :: UVHandleType
pattern UV_PROCESS = #const UV_PROCESS
pattern UV_STREAM :: UVHandleType
pattern UV_STREAM = #const UV_STREAM
pattern UV_TCP :: UVHandleType
pattern UV_TCP = #const UV_TCP
pattern UV_TIMER :: UVHandleType
pattern UV_TIMER = #const UV_TIMER
pattern UV_TTY :: UVHandleType
pattern UV_TTY = #const UV_TTY
pattern UV_UDP :: UVHandleType
pattern UV_UDP = #const UV_UDP
pattern UV_SIGNAL :: UVHandleType
pattern UV_SIGNAL = #const UV_SIGNAL
pattern UV_FILE :: UVHandleType
pattern UV_FILE = #const UV_FILE

foreign import ccall unsafe uv_guess_handle :: UVFD -> IO UVHandleType

foreign import ccall unsafe uv_resident_set_memory :: MBA## CSize -> IO CInt
foreign import ccall unsafe uv_uptime :: MBA## Double -> IO CInt
foreign import ccall unsafe uv_getrusage :: MBA## a -> IO CInt

-- | Data type for storing times.
-- typedef struct { long tv_sec; long tv_usec; } uv_timeval_t;
data TimeVal = TimeVal 
    { tv_sec  :: {-# UNPACK #-} !CLong
    , tv_usec :: {-# UNPACK #-} !CLong
    }   deriving (Show, Read, Eq, Ord, Generic)
        deriving anyclass (ShowT, EncodeJSON, ToValue, FromValue)

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
        deriving anyclass (ShowT, EncodeJSON, ToValue, FromValue)

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

newtype PID = PID CInt 
    deriving (Eq, Ord, Show, Generic)
    deriving newtype (Storable, Prim, Unaligned, EncodeJSON, ToValue, FromValue)
    deriving anyclass ShowT

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

data OSName = OSName
    { os_sysname :: T.Text
    , os_release :: T.Text
    , os_version :: T.Text
    , os_machine :: T.Text
    }   deriving (Eq, Ord, Show, Generic)
        deriving anyclass (ShowT, EncodeJSON, ToValue, FromValue)

getOSName :: IO OSName
getOSName = do
    (A.MutablePrimArray mba## :: A.MutablePrimArray A.RealWorld Word8) <- A.newArr (#size uv_utsname_t)
    throwUVIfMinus_ (uv_os_uname mba##)
    sn <- toText <$> peekMBA mba## (#offset uv_utsname_t, sysname)
    re <- toText <$> peekMBA mba## (#offset uv_utsname_t, release)
    ve <- toText <$> peekMBA mba## (#offset uv_utsname_t, version)
    ma <- toText <$> peekMBA mba##  (#offset uv_utsname_t, machine) 
    return (OSName sn re ve ma)
    
foreign import ccall unsafe uv_os_uname :: MBA## OSName -> IO CInt

foreign import ccall unsafe hs_uv_random :: MBA## Word8 -> CSize -> CInt -> IO CInt
foreign import ccall unsafe hs_uv_random_threaded :: Ptr Word8 -> CSize -> CInt -> Ptr UVLoop -> IO UVSlotUnsafe
