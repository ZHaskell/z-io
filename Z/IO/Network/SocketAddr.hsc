{-|
Module      : Z.IO.Network.SocketAddr
Description : TCP/UDP socket address API
Copyright   : (c) Winterland, 2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

This module provides necessary types and constant for low level socket address manipulating.

-}

module Z.IO.Network.SocketAddr
  ( -- * name to address
    SocketAddr(..)
  , ipv4, ipv6
  , sockAddrFamily
  , withSocketAddr
  , withSocketAddrUnsafe
  , sizeOfSocketAddr
  , withSocketAddrStorage
  , withSocketAddrStorageUnsafe
  , sizeOfSocketAddrStorage
   -- ** IPv4 address
  , InetAddr(..)
  , inetAny
  , inetBroadcast
  , inetNone
  , inetLoopback
  , inetUnspecificGroup
  , inetAllHostsGroup
  , inetMaxLocalGroup
  , inetAddrToTuple
  , tupleToInetAddr
   -- ** IPv6 address
  , Inet6Addr(..)
  , inet6Any
  , inet6Loopback
  , inet6AddrToTuple
  , tupleToInet6Addr
  , FlowInfo
  , ScopeID
  -- * port numbber
  , PortNumber(..)
  , portAny
  -- * family, type, protocol
  , SocketFamily(AF_UNSPEC, AF_INET, AF_INET6)
  , SocketType(SOCK_DGRAM, SOCK_STREAM, SOCK_SEQPACKET, SOCK_RAW, SOCK_RDM, SOCK_ANY)
  , ProtocolNumber(IPPROTO_DEFAULT, IPPROTO_IP, IPPROTO_TCP, IPPROTO_UDP)
  -- * Internal helper
  , peekSocketAddr
  , pokeSocketAddr
  , peekSocketAddrMBA
  , pokeSocketAddrMBA
  , htons
  , ntohs
  , ntohl
  , htonl
  ) where

import           Data.Bits
import qualified Data.List                as List
import           Data.Typeable
import           Foreign
import           Foreign.C
import           GHC.Generics
import           Numeric                    (showHex)
import           System.IO.Unsafe
import           Z.Data.CBytes
import           Z.Data.Text.ShowT          (ShowT)
import           Z.IO.Exception
import           Z.IO.UV.Errno
import           Z.Foreign

#include "hs_uv.h" 

#if defined(i386_HOST_ARCH) && defined(mingw32_HOST_OS)
#let CALLCONV = "stdcall"
#else
#let CALLCONV = "ccall"
#endif

--------------------------------------------------------------------------------

#if defined(_WIN32)
type CSaFamily = (#type unsigned short)
#elif defined(darwin_HOST_OS)
type CSaFamily = (#type u_char)
#else
type CSaFamily = (#type sa_family_t)
#endif

-- | IPv4 or IPv6 socket address, i.e. the `sockaddr_in` or `sockaddr_in6` struct.
-- 
data SocketAddr 
    = SocketAddrInet
        {-# UNPACK #-} !PortNumber  -- sin_port  (network byte order)
        {-# UNPACK #-} !InetAddr    -- sin_addr  (ditto)
    | SocketAddrInet6
        {-# UNPACK #-} !PortNumber  -- sin6_port (network byte order)
        {-# UNPACK #-} !FlowInfo    -- sin6_flowinfo (ditto)
        {-# UNPACK #-} !Inet6Addr   -- sin6_addr (ditto)
        {-# UNPACK #-} !ScopeID     -- sin6_scope_id (ditto)
  deriving (Eq, Ord, Typeable)

instance Show SocketAddr where
    showsPrec _ (SocketAddrInet port ia)
       = shows ia . showString ":" . shows port
    showsPrec _ (SocketAddrInet6 port _ ia6 _)
       = ('[':) . shows ia6 . showString "]:" . shows port

sockAddrFamily :: SocketAddr -> SocketFamily
sockAddrFamily (SocketAddrInet _ _) = AF_INET
sockAddrFamily (SocketAddrInet6 _ _ _ _) = AF_INET6

type FlowInfo = Word32
type ScopeID = Word32

-- | Convert a string containing an IPv4 addresses to a binary structure
--
-- This is partial function, wrong address will throw 'InvalidArgument' exception.
ipv4:: HasCallStack => CBytes -> PortNumber -> SocketAddr
ipv4 str (PortNumber port) = unsafeDupablePerformIO . withSocketAddrStorageUnsafe $ \ p ->
    withCBytesUnsafe str $ \ cstr -> throwUVIfMinus_ $ uv_ip4_addr cstr (fromIntegral port) p

-- | Convert a string containing an IPv6 addresses to a binary structure
--
-- This is partial function, wrong address will throw 'InvalidArgument' exception.
ipv6:: HasCallStack => CBytes -> PortNumber -> SocketAddr
ipv6 str (PortNumber port) = unsafeDupablePerformIO . withSocketAddrStorageUnsafe $ \ p ->
    withCBytesUnsafe str $ \ cstr -> throwUVIfMinus_ $ uv_ip6_addr cstr (fromIntegral port) p

--------------------------------------------------------------------------------

-- | Independent of endianness. For example @127.0.0.1@ is stored as @(127, 0, 0, 1)@.
--
-- For direct manipulation prefer 'inetAddrToTuple' and 'tupleToInetAddr'.
--
newtype InetAddr = InetAddr { getInetAddr :: Word32 } deriving (Eq, Ord, Typeable)
instance Show InetAddr where
    showsPrec _ ia = 
        let (a,b,c,d) = inetAddrToTuple ia
        in shows a . ('.':) . shows b . ('.':) . shows c . ('.':) . shows d 

-- | @0.0.0.0@
inetAny             :: InetAddr
inetAny              = InetAddr 0

-- | @255.255.255.255@
inetBroadcast       :: InetAddr
inetBroadcast        = tupleToInetAddr (255,255,255,255)

-- | @255.255.255.255@
inetNone            :: InetAddr
inetNone             = tupleToInetAddr (255,255,255,255)

-- | @127.0.0.1@
inetLoopback        :: InetAddr
inetLoopback         = tupleToInetAddr (127,  0,  0,  1)

-- | @224.0.0.0@
inetUnspecificGroup :: InetAddr
inetUnspecificGroup  = tupleToInetAddr (224,  0,  0,  0)

-- | @224.0.0.1@
inetAllHostsGroup   :: InetAddr
inetAllHostsGroup    = tupleToInetAddr (224,  0,  0,  1)

-- | @224.0.0.255@
inetMaxLocalGroup   :: InetAddr
inetMaxLocalGroup    = tupleToInetAddr (224,  0,  0,255)

instance Storable InetAddr where
    sizeOf _ = 4
    alignment _ = alignment (undefined :: Word32) 
    peek p = (InetAddr . ntohl) `fmap` peekByteOff p 0
    poke p (InetAddr ia) = pokeByteOff p 0 (htonl ia)

instance Unaligned InetAddr where
    unalignedSize _ = 4
    pokeMBA p off x = pokeMBA p off (htonl (getInetAddr x))
    peekMBA p off = InetAddr . ntohl <$> peekMBA p off
    indexBA p off = InetAddr (ntohl (indexBA p off))
    
-- | Converts 'InetAddr' to representation-independent IPv4 quadruple.
-- For example for @127.0.0.1@ the function will return @(127, 0, 0, 1)@
-- regardless of host endianness.
inetAddrToTuple :: InetAddr -> (Word8, Word8, Word8, Word8)
inetAddrToTuple (InetAddr ia) =
    let byte i = fromIntegral (ia `shiftR` i) :: Word8
    in (byte 24, byte 16, byte 8, byte 0)

-- | Converts IPv4 quadruple to 'InetAddr'.
tupleToInetAddr :: (Word8, Word8, Word8, Word8) -> InetAddr
tupleToInetAddr (b3, b2, b1, b0) =
    let x `sl` i = fromIntegral x `shiftL` i :: Word32
    in InetAddr $ (b3 `sl` 24) .|. (b2 `sl` 16) .|. (b1 `sl` 8) .|. (b0 `sl` 0)

--------------------------------------------------------------------------------

-- | Independent of endianness. For example @::1@ is stored as @(0, 0, 0, 1)@.
--
-- For direct manipulation prefer 'inet6AddrToTuple' and 'tupleToInet6Addr'.
--
data Inet6Addr = Inet6Addr {-# UNPACK #-}!Word32
                           {-# UNPACK #-}!Word32
                           {-# UNPACK #-}!Word32
                           {-# UNPACK #-}!Word32 deriving (Eq, Ord, Typeable)


instance Show Inet6Addr where
    showsPrec _ ia6@(Inet6Addr a1 a2 a3 a4)
        -- IPv4-Mapped IPv6 Address
        | a1 == 0 && a2 == 0 && a3 == 0xffff =
          showString "::ffff:" . shows (InetAddr a4)
        -- IPv4-Compatible IPv6 Address (exclude IPRange ::/112)
        | a1 == 0 && a2 == 0 && a3 == 0 && a4 >= 0x10000 =
            showString "::" . shows (InetAddr a4)
        -- length of longest run > 1, replace it with "::"
        | end - begin > 1 =
            showFields prefix . showString "::" . showFields suffix
        | otherwise =
            showFields fields
      where
        fields =
            let (u7, u6, u5, u4, u3, u2, u1, u0) = inet6AddrToTuple ia6 in
            [u7, u6, u5, u4, u3, u2, u1, u0]
        showFields = foldr (.) id . List.intersperse (':':) . map showHex
        prefix = take begin fields  -- fields before "::"
        suffix = drop end fields    -- fields after "::"
        begin = end + diff          -- the longest run of zeros
        (diff, end) = minimum $
            scanl (\c i -> if i == 0 then c - 1 else 0) 0 fields `zip` [0..]

-- | @::@
inet6Any      :: Inet6Addr
inet6Any       = Inet6Addr 0 0 0 0

-- | @::1@
inet6Loopback :: Inet6Addr
inet6Loopback  = Inet6Addr 0 0 0 1

-- | convert 'Inet6Addr' to octets.
inet6AddrToTuple :: Inet6Addr -> (Word16, Word16, Word16, Word16,
                                        Word16, Word16, Word16, Word16)
inet6AddrToTuple (Inet6Addr w3 w2 w1 w0) =
    let high, low :: Word32 -> Word16
        high w = fromIntegral (w `shiftR` 16)
        low w = fromIntegral w
    in (high w3, low w3, high w2, low w2, high w1, low w1, high w0, low w0)

-- | convert 'Inet6Addr' from octets.
tupleToInet6Addr :: (Word16, Word16, Word16, Word16,
                        Word16, Word16, Word16, Word16) -> Inet6Addr
tupleToInet6Addr (w7, w6, w5, w4, w3, w2, w1, w0) =
    let add :: Word16 -> Word16 -> Word32
        high `add` low = (fromIntegral high `shiftL` 16) .|. (fromIntegral low)
    in  Inet6Addr (w7 `add` w6) (w5 `add` w4) (w3 `add` w2) (w1 `add` w0)

instance Storable Inet6Addr where
    sizeOf _    = #size struct in6_addr
    alignment _ = #alignment struct in6_addr
    peek p = do
        a <- peek32 p 0
        b <- peek32 p 1
        c <- peek32 p 2
        d <- peek32 p 3
        return $ Inet6Addr a b c d
    poke p (Inet6Addr a b c d) = do
        poke32 p 0 a
        poke32 p 1 b
        poke32 p 2 c
        poke32 p 3 d


s6_addr_offset :: Int
s6_addr_offset = (#offset struct in6_addr, s6_addr)

peek32 :: Ptr a -> Int -> IO Word32
peek32 p i0 = do
    let i' = i0 * 4
        peekByte n = peekByteOff p (s6_addr_offset + i' + n) :: IO Word8
        a `sl` i = fromIntegral a `shiftL` i
    a0 <- peekByte 0
    a1 <- peekByte 1
    a2 <- peekByte 2
    a3 <- peekByte 3
    return ((a0 `sl` 24) .|. (a1 `sl` 16) .|. (a2 `sl` 8) .|. (a3 `sl` 0))

poke32 :: Ptr a -> Int -> Word32 -> IO ()
poke32 p i0 a = do
    let i' = i0 * 4
        pokeByte n = pokeByteOff p (s6_addr_offset + i' + n)
        x `sr` i = fromIntegral (x `shiftR` i) :: Word8
    pokeByte 0 (a `sr` 24)
    pokeByte 1 (a `sr` 16)
    pokeByte 2 (a `sr`  8)
    pokeByte 3 (a `sr`  0)

instance Unaligned Inet6Addr where
    unalignedSize _ = (#size struct in6_addr)

    indexBA p off = 
        let a = indexBA p (off + s6_addr_offset + 0)
            b = indexBA p  (off + s6_addr_offset + 4)
            c = indexBA p  (off + s6_addr_offset + 8)
            d = indexBA p  (off + s6_addr_offset + 12)
        in Inet6Addr (getBE a) (getBE b) (getBE c) (getBE d)

    peekMBA p off = do
        a <- peekMBA p (off + s6_addr_offset + 0)
        b <- peekMBA p  (off + s6_addr_offset + 4)
        c <- peekMBA p  (off + s6_addr_offset + 8)
        d <- peekMBA p  (off + s6_addr_offset + 12)
        return $ Inet6Addr (getBE a) (getBE b) (getBE c) (getBE d)

    pokeMBA p off (Inet6Addr a b c d) = do
        pokeMBA p (off + s6_addr_offset) (BE a)
        pokeMBA p (off + 4 + s6_addr_offset) (BE b)
        pokeMBA p (off + 8 + s6_addr_offset) (BE c)
        pokeMBA p (off + 12 + s6_addr_offset) (BE d)

--------------------------------------------------------------------------------

peekSocketAddr :: HasCallStack => Ptr SocketAddr -> IO SocketAddr
peekSocketAddr p = do
    family <- (#peek struct sockaddr, sa_family) p
    case family :: CSaFamily of
        (#const AF_INET) -> do
            addr <- (#peek struct sockaddr_in, sin_addr) p
            port <- (#peek struct sockaddr_in, sin_port) p
            return (SocketAddrInet port addr)
        (#const AF_INET6) -> do
            port <- (#peek struct sockaddr_in6, sin6_port) p
            flow <- (#peek struct sockaddr_in6, sin6_flowinfo) p
            addr <- (#peek struct sockaddr_in6, sin6_addr) p
            scope <- (#peek struct sockaddr_in6, sin6_scope_id) p
            return (SocketAddrInet6 port flow addr scope)
        _ -> do let errno = UV_EAI_ADDRFAMILY
                name <- uvErrName errno
                desc <- uvStdError errno
                throwUVError errno (IOEInfo name desc callStack)

pokeSocketAddr :: Ptr SocketAddr -> SocketAddr -> IO ()
pokeSocketAddr p (SocketAddrInet port addr) =  do
#if defined(darwin_HOST_OS)
    clearPtr p (#size struct sockaddr_in)
#endif
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
    (#poke struct sockaddr_in, sin_len) p ((#size struct sockaddr_in) :: Word8)
#endif
    (#poke struct sockaddr_in, sin_family) p ((#const AF_INET) :: CSaFamily)
    (#poke struct sockaddr_in, sin_port) p port
    (#poke struct sockaddr_in, sin_addr) p addr
pokeSocketAddr p (SocketAddrInet6 port flow addr scope) =  do
#if defined(darwin_HOST_OS)
    clearPtr p (#size struct sockaddr_in6)
#endif
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
    (#poke struct sockaddr_in6, sin6_len) p ((#size struct sockaddr_in6) :: Word8)
#endif
    (#poke struct sockaddr_in6, sin6_family) p ((#const AF_INET6) :: CSaFamily)
    (#poke struct sockaddr_in6, sin6_port) p port
    (#poke struct sockaddr_in6, sin6_flowinfo) p flow
    (#poke struct sockaddr_in6, sin6_addr) p (addr)
    (#poke struct sockaddr_in6, sin6_scope_id) p scope


-- | Pass 'SocketAddr' to FFI as pointer.
--
withSocketAddr :: SocketAddr -> (Ptr SocketAddr -> IO a) -> IO a
withSocketAddr sa@(SocketAddrInet _ _) f = do
    allocaBytesAligned
        (#size struct sockaddr_in)
        (#alignment struct sockaddr_in) $ \ p -> pokeSocketAddr p sa >> f p
withSocketAddr sa@(SocketAddrInet6 _ _ _ _) f = do
    allocaBytesAligned 
        (#size struct sockaddr_in6) 
        (#alignment struct sockaddr_in6) $ \ p -> pokeSocketAddr p sa >> f p

-- | Pass 'SocketAddr' to FFI as pointer.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withSocketAddrUnsafe :: SocketAddr -> (MBA## SocketAddr -> IO a) -> IO a
withSocketAddrUnsafe sa@(SocketAddrInet _ _) f = do
    (MutableByteArray p) <- newByteArray (#size struct sockaddr_in) 
    pokeSocketAddrMBA p sa 
    f p
withSocketAddrUnsafe sa@(SocketAddrInet6 _ _ _ _) f = do
    (MutableByteArray p) <- newByteArray (#size struct sockaddr_in6) 
    pokeSocketAddrMBA p sa 
    f p

sizeOfSocketAddr :: SocketAddr -> CSize
sizeOfSocketAddr (SocketAddrInet _ _) = #size struct sockaddr_in
sizeOfSocketAddr (SocketAddrInet6 _ _ _ _) = #size struct sockaddr_in6

-- | Allocate space for 'sockaddr_storage' and pass to FFI.
withSocketAddrStorage :: (Ptr SocketAddr -> IO ()) -> IO SocketAddr
withSocketAddrStorage f = do
    allocaBytesAligned
        (#size struct sockaddr_storage)
        (#alignment struct sockaddr_storage) $ \ p -> f p >> peekSocketAddr p

-- | Allocate space for 'sockaddr_storage' and pass to FFI.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withSocketAddrStorageUnsafe :: (MBA## SocketAddr -> IO ()) -> IO SocketAddr
withSocketAddrStorageUnsafe f = do
    (MutableByteArray p) <- newByteArray (#size struct sockaddr_storage) 
    f p
    peekSocketAddrMBA p

sizeOfSocketAddrStorage :: CSize
sizeOfSocketAddrStorage = (#size struct sockaddr_storage)

peekSocketAddrMBA :: HasCallStack => MBA## SocketAddr -> IO SocketAddr
peekSocketAddrMBA p = do
    family <- peekMBA p (#offset struct sockaddr, sa_family)
    case family :: CSaFamily of
        (#const AF_INET) -> do
            addr <- peekMBA p (#offset struct sockaddr_in, sin_addr) 
            port <- peekMBA p (#offset struct sockaddr_in, sin_port) 
            return (SocketAddrInet port addr)
        (#const AF_INET6) -> do
            port <- peekMBA p (#offset struct sockaddr_in6, sin6_port) 
            flow <- peekMBA p (#offset struct sockaddr_in6, sin6_flowinfo) 
            addr <- peekMBA p (#offset struct sockaddr_in6, sin6_addr) 
            scope <- peekMBA p (#offset struct sockaddr_in6, sin6_scope_id) 
            return (SocketAddrInet6 port flow addr scope)
        _ -> do let errno = UV_EAI_ADDRFAMILY
                name <- uvErrName errno
                desc <- uvStdError errno
                throwUVError errno (IOEInfo name desc callStack)

pokeSocketAddrMBA :: MBA## SocketAddr -> SocketAddr -> IO ()
pokeSocketAddrMBA p (SocketAddrInet port addr) =  do
#if defined(darwin_HOST_OS)
    clearMBA p (#size struct sockaddr_in)
#endif
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
    pokeMBA p (#offset struct sockaddr_in, sin_len) ((#size struct sockaddr_in) :: Word8)
#endif
    pokeMBA p (#offset struct sockaddr_in, sin_family) ((#const AF_INET) :: CSaFamily)
    pokeMBA p (#offset struct sockaddr_in, sin_port) port
    pokeMBA p (#offset struct sockaddr_in, sin_addr) addr
pokeSocketAddrMBA p (SocketAddrInet6 port flow addr scope) =  do
#if defined(darwin_HOST_OS)
    clearMBA p (#size struct sockaddr_in6)
#endif
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
    pokeMBA p (#offset struct sockaddr_in6, sin6_len) ((#size struct sockaddr_in6) :: Word8)
#endif
    pokeMBA p (#offset struct sockaddr_in6, sin6_family) ((#const AF_INET6) :: CSaFamily)
    pokeMBA p (#offset struct sockaddr_in6, sin6_port) port
    pokeMBA p (#offset struct sockaddr_in6, sin6_flowinfo) flow
    pokeMBA p (#offset struct sockaddr_in6, sin6_addr) (addr)
    pokeMBA p (#offset struct sockaddr_in6, sin6_scope_id) scope

--------------------------------------------------------------------------------
-- Port Numbers

-- | Port number.
--   Use the @Num@ instance (i.e. use a literal) to create a
--   @PortNumber@ value.
--
-- >>> 1 :: PortNumber
-- 1
-- >>> read "1" :: PortNumber
-- 1
-- >>> show (12345 :: PortNumber)
-- "12345"
-- >>> 50000 < (51000 :: PortNumber)
-- True
-- >>> 50000 < (52000 :: PortNumber)
-- True
-- >>> 50000 + (10000 :: PortNumber)
-- 60000
newtype PortNumber = PortNumber Word16 deriving (Eq, Ord, Enum, Generic)
                                        deriving newtype (Show, Read, Num, Bounded, Real, Integral)
                                        deriving anyclass ShowT

-- | @:0@
portAny :: PortNumber
portAny = PortNumber 0

instance Storable PortNumber where
   sizeOf    _ = sizeOf    (0 :: Word16)
   alignment _ = alignment (0 :: Word16)
   poke p (PortNumber po) = poke (castPtr p) (htons po)
   peek p = PortNumber . ntohs <$> peek (castPtr p)

instance Unaligned PortNumber where
   unalignedSize _ = 2
   indexBA p off = PortNumber . ntohs $ indexBA p off
   pokeMBA p off (PortNumber po) = pokeMBA p off (htons po)
   peekMBA p off = PortNumber . ntohs <$> peekMBA p off
    
--------------------------------------------------------------------------------

newtype SocketFamily = SocketFamily CInt
    deriving (Eq, Ord, Read, Show, Typeable)
newtype SocketType = SocketType CInt
    deriving (Eq, Ord, Read, Show, Typeable)
newtype ProtocolNumber = ProtocolNumber CInt
    deriving (Eq, Ord, Read, Show, Typeable)

instance Storable SocketFamily where                      
    sizeOf _ = sizeOf (undefined :: CInt)       
    alignment _ = alignment (undefined :: CInt) 
    peek ptr = SocketFamily `fmap` peek (castPtr ptr)             
    poke ptr (SocketFamily v) = poke (castPtr ptr) v

instance Storable SocketType where                      
    sizeOf _ = sizeOf (undefined :: CInt)       
    alignment _ = alignment (undefined :: CInt) 
    peek ptr = SocketType `fmap` peek (castPtr ptr)             
    poke ptr (SocketType v) = poke (castPtr ptr) v

instance Storable ProtocolNumber where                      
    sizeOf _ = sizeOf (undefined :: CInt)       
    alignment _ = alignment (undefined :: CInt) 
    peek ptr = ProtocolNumber `fmap` peek (castPtr ptr)             
    poke ptr (ProtocolNumber v) = poke (castPtr ptr) v

-- | unspecified
pattern AF_UNSPEC :: SocketFamily
pattern AF_UNSPEC = SocketFamily (#const AF_UNSPEC)
-- | internetwork: UDP, TCP, etc
pattern AF_INET :: SocketFamily
pattern AF_INET = SocketFamily (#const AF_INET)
-- | Internet Protocol version 6
pattern AF_INET6 :: SocketFamily
pattern AF_INET6 = SocketFamily (#const AF_INET6)

pattern SOCK_STREAM :: SocketType
pattern SOCK_STREAM = SocketType (#const SOCK_STREAM)
pattern SOCK_DGRAM :: SocketType
pattern SOCK_DGRAM = SocketType (#const SOCK_DGRAM)
pattern SOCK_RAW :: SocketType
pattern SOCK_RAW = SocketType (#const SOCK_RAW)
pattern SOCK_RDM :: SocketType
pattern SOCK_RDM = SocketType (#const SOCK_RDM)
pattern SOCK_SEQPACKET :: SocketType
pattern SOCK_SEQPACKET = SocketType (#const SOCK_SEQPACKET)
-- | Used in getAddrInfo hints, for any type can be returned by getAddrInfo
pattern SOCK_ANY :: SocketType
pattern SOCK_ANY = SocketType 0

pattern IPPROTO_DEFAULT :: ProtocolNumber
pattern IPPROTO_DEFAULT = ProtocolNumber 0
pattern IPPROTO_IP :: ProtocolNumber
pattern IPPROTO_IP = ProtocolNumber (#const IPPROTO_IP)
pattern IPPROTO_TCP :: ProtocolNumber
pattern IPPROTO_TCP = ProtocolNumber (#const IPPROTO_TCP)
pattern IPPROTO_UDP :: ProtocolNumber
pattern IPPROTO_UDP = ProtocolNumber (#const IPPROTO_UDP)

--------------------------------------------------------------------------------

foreign import #{CALLCONV} unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import #{CALLCONV} unsafe "htons" htons :: Word16 -> Word16
foreign import #{CALLCONV} unsafe "ntohl" ntohl :: Word32 -> Word32
foreign import #{CALLCONV} unsafe "htonl" htonl :: Word32 -> Word32

foreign import ccall unsafe uv_ip4_addr :: BA## Word8 -> CInt -> MBA## SocketAddr -> IO CInt
foreign import ccall unsafe uv_ip6_addr :: BA## Word8 -> CInt -> MBA## SocketAddr -> IO CInt
