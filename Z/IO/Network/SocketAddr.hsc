{-|
Module      : Z.IO.Network.SocketAddr
Description : TCP\/UDP socket address API
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
  , IPv4(..)
  , ipv4Any
  , ipv4Broadcast
  , ipv4None
  , ipv4Loopback
  , ipv4UnspecificGroup
  , ipv4AllHostsGroup
  , ipv4MaxLocalGroup
  , ipv4AddrToTuple
  , tupleToIPv4Addr
   -- ** IPv6 address
  , IPv6(..)
  , ipv6Any
  , ipv6Loopback
  , ipv6AddrToTuple
  , tupleToIPv6Addr
  , FlowInfo
  , ScopeID
  -- * port numbber
  , PortNumber(..)
  , portAny
  -- * family, type, protocol
  -- ** SocketFamily
  , SocketFamily
  , pattern AF_UNSPEC
  , pattern AF_INET
  , pattern AF_INET6
  , SocketType
  , pattern SOCK_DGRAM
  , pattern SOCK_STREAM
  , pattern SOCK_SEQPACKET
  , pattern SOCK_RAW
  , pattern SOCK_RDM
  , pattern SOCK_ANY
  , ProtocolNumber
  , pattern IPPROTO_DEFAULT
  , pattern IPPROTO_IP
  , pattern IPPROTO_TCP
  , pattern IPPROTO_UDP
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
import           Foreign
import           Foreign.C
import           GHC.Generics
import           Numeric                (showHex)
import           System.IO.Unsafe
import           Z.Data.CBytes
import           Z.Data.Text.ShowT      (ShowT(..))
import qualified Z.Data.Text.ShowT      as T
import           Z.Data.JSON            (EncodeJSON(..), ToValue(..), FromValue(..), (.:))
import qualified Z.Data.JSON            as JSON
import qualified Z.Data.JSON.Builder    as B
import qualified Z.Data.Vector          as V
import qualified Z.Data.Vector.Extra    as V
import           Z.IO.Exception
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
-- Example on JSON instance:
--
-- @
-- > JSON.encodeText  $ ipv6 "3731:54:65fe:2::a8" 9090
-- "{\"addr\":[14129,84,26110,2,0,0,0,168],\"port\":9090,\"flow\":0,\"scope\":0}"
-- > JSON.encodeText  $ ipv4 "128.14.32.1" 9090
-- "{\"addr\":[128,14,32,1],\"port\":9090}"
-- @
data SocketAddr 
    = SocketAddrIPv4
        {-# UNPACK #-} !IPv4    -- sin_addr  (ditto)
        {-# UNPACK #-} !PortNumber  -- sin_port  (network byte order)
    | SocketAddrIPv6
        {-# UNPACK #-} !IPv6   -- sin6_addr (ditto)
        {-# UNPACK #-} !PortNumber  -- sin6_port (network byte order)
        {-# UNPACK #-} !FlowInfo    -- sin6_flowinfo (ditto)
        {-# UNPACK #-} !ScopeID     -- sin6_scope_id (ditto)
    deriving (Eq, Ord, Generic)

instance EncodeJSON SocketAddr where 
    encodeJSON (SocketAddrIPv4 addr port) = T.curly $ do
        "addr" `B.kv` encodeJSON addr
        T.char7 ','
        "port" `B.kv` encodeJSON port
    encodeJSON (SocketAddrIPv6 addr port flow scope) = T.curly $ do
        "addr" `B.kv` encodeJSON addr
        T.char7 ','
        "port" `B.kv` encodeJSON port
        T.char7 ','
        "flow" `B.kv` encodeJSON flow
        T.char7 ','
        "scope" `B.kv` encodeJSON scope

instance ToValue SocketAddr where 
    toValue (SocketAddrIPv4 addr port) = JSON.Object . V.pack $ 
        [ ("addr", toValue addr)
        , ("number", toValue port)
        ]
    toValue (SocketAddrIPv6 addr port flow scope) = JSON.Object . V.pack $ 
        [ ("addr", toValue addr)
        , ("number", toValue port)
        , ("flow", toValue flow)
        , ("scope", toValue scope)
        ]
instance FromValue SocketAddr where 
    fromValue = JSON.withFlatMapR "Z.IO.Network.SocketAddr" $ \ fm -> do
        (addrV :: V.PrimVector Word) <- fm .: "addr"
        case V.length addrV of
            4 -> do port <- fm .: "port"
                    let a = fromIntegral $ addrV `V.unsafeIndex` 0
                        b = fromIntegral $ addrV `V.unsafeIndex` 1
                        c = fromIntegral $ addrV `V.unsafeIndex` 2
                        d = fromIntegral $ addrV `V.unsafeIndex` 3
                        !addr = tupleToIPv4Addr (a,b,c,d)
                    return (SocketAddrIPv4 addr port)
            8 -> do port <- fm .: "port"
                    flow <- fm .: "flow"
                    scope <- fm .: "scope"
                    let a = fromIntegral $ addrV `V.unsafeIndex` 0
                        b = fromIntegral $ addrV `V.unsafeIndex` 1
                        c = fromIntegral $ addrV `V.unsafeIndex` 2
                        d = fromIntegral $ addrV `V.unsafeIndex` 3
                        e = fromIntegral $ addrV `V.unsafeIndex` 4
                        f = fromIntegral $ addrV `V.unsafeIndex` 5
                        g = fromIntegral $ addrV `V.unsafeIndex` 6
                        h = fromIntegral $ addrV `V.unsafeIndex` 7
                        !addr = tupleToIPv6Addr (a,b,c,d,e,f,g,h)
                    return (SocketAddrIPv6 addr port flow scope)
            _ -> JSON.fail' "wrong address length"

instance Show SocketAddr where show = T.toString

instance ShowT SocketAddr where
    toUTF8BuilderP _ (SocketAddrIPv4 addr port)
       = T.toUTF8Builder addr >> T.char7 ':' >> T.toUTF8Builder port
    toUTF8BuilderP _ (SocketAddrIPv6 addr port _ _) = do
        T.square (T.toUTF8Builder addr)
        T.char7 ':' 
        T.toUTF8Builder port

sockAddrFamily :: SocketAddr -> SocketFamily
sockAddrFamily (SocketAddrIPv4 _ _) = AF_INET
sockAddrFamily (SocketAddrIPv6 _ _ _ _) = AF_INET6

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
-- For direct manipulation prefer 'ipv4AddrToTuple' and 'tupleToIPv4Addr'.
--
-- JSON instance encode ipv4 address into an array with 4 'Word8' octets.
newtype IPv4 = IPv4 { getIPv4Addr :: Word32 }
    deriving (Eq, Ord, Generic)
    
instance EncodeJSON IPv4 where
    {-# INLINE encodeJSON #-}
    encodeJSON = encodeJSON . ipv4AddrToTuple
instance ToValue IPv4 where
    {-# INLINE toValue #-}
    toValue = toValue . ipv4AddrToTuple
instance FromValue IPv4 where
    {-# INLINE fromValue #-}
    fromValue v = tupleToIPv4Addr <$> fromValue v

instance Show IPv4 where show = T.toString
instance ShowT IPv4 where
    toUTF8BuilderP _ ia = do
        let (a,b,c,d) = ipv4AddrToTuple ia
        T.int a 
        T.char7 '.' 
        T.int b  
        T.char7 '.' 
        T.int c
        T.char7 '.' 
        T.int d  

-- | @0.0.0.0@
ipv4Any             :: IPv4
ipv4Any              = IPv4 0

-- | @255.255.255.255@
ipv4Broadcast       :: IPv4
ipv4Broadcast        = tupleToIPv4Addr (255,255,255,255)

-- | @255.255.255.255@
ipv4None            :: IPv4
ipv4None             = tupleToIPv4Addr (255,255,255,255)

-- | @127.0.0.1@
ipv4Loopback        :: IPv4
ipv4Loopback         = tupleToIPv4Addr (127,  0,  0,  1)

-- | @224.0.0.0@
ipv4UnspecificGroup :: IPv4
ipv4UnspecificGroup  = tupleToIPv4Addr (224,  0,  0,  0)

-- | @224.0.0.1@
ipv4AllHostsGroup   :: IPv4
ipv4AllHostsGroup    = tupleToIPv4Addr (224,  0,  0,  1)

-- | @224.0.0.255@
ipv4MaxLocalGroup   :: IPv4
ipv4MaxLocalGroup    = tupleToIPv4Addr (224,  0,  0,255)

instance Storable IPv4 where
    sizeOf _ = 4
    alignment _ = alignment (undefined :: Word32) 
    peek p = (IPv4 . ntohl) `fmap` peekByteOff p 0
    poke p (IPv4 ia) = pokeByteOff p 0 (htonl ia)

instance Unaligned IPv4 where
    unalignedSize _ = 4
    pokeMBA p off x = pokeMBA p off (htonl (getIPv4Addr x))
    peekMBA p off = IPv4 . ntohl <$> peekMBA p off
    indexBA p off = IPv4 (ntohl (indexBA p off))
    
-- | Converts 'IPv4' to representation-independent IPv4 quadruple.
-- For example for @127.0.0.1@ the function will return @(127, 0, 0, 1)@
-- regardless of host endianness.
ipv4AddrToTuple :: IPv4 -> (Word8, Word8, Word8, Word8)
ipv4AddrToTuple (IPv4 ia) =
    let byte i = fromIntegral (ia `shiftR` i) :: Word8
    in (byte 24, byte 16, byte 8, byte 0)

-- | Converts IPv4 quadruple to 'IPv4'.
tupleToIPv4Addr :: (Word8, Word8, Word8, Word8) -> IPv4
tupleToIPv4Addr (b3, b2, b1, b0) =
    let x `sl` i = fromIntegral x `shiftL` i :: Word32
    in IPv4 $ (b3 `sl` 24) .|. (b2 `sl` 16) .|. (b1 `sl` 8) .|. (b0 `sl` 0)

--------------------------------------------------------------------------------

-- | Independent of endianness. For example @::1@ is stored as @(0, 0, 0, 1)@.
--
-- For direct manipulation prefer 'ipv6AddrToTuple' and 'tupleToIPv6Addr'.
--
-- JSON instance encode ipv6 address into an array with 8 'Word16' octets.
data IPv6 = IPv6 {-# UNPACK #-}!Word32
                 {-# UNPACK #-}!Word32
                 {-# UNPACK #-}!Word32
                 {-# UNPACK #-}!Word32 
    deriving (Eq, Ord, Generic)

instance EncodeJSON IPv6 where
    {-# INLINE encodeJSON #-}
    encodeJSON addr = encodeJSON [a,b,c,d,e,f,g,h]
      where (a,b,c,d,e,f,g,h) = ipv6AddrToTuple addr
instance ToValue IPv6 where
    {-# INLINE toValue #-}
    toValue addr = toValue [a,b,c,d,e,f,g,h]
      where (a,b,c,d,e,f,g,h) = ipv6AddrToTuple addr
instance FromValue IPv6 where
    {-# INLINE fromValue #-}
    fromValue v = do
        [a,b,c,d,e,f,g,h] <- fromValue v
        return $! tupleToIPv6Addr (a,b,c,d,e,f,g,h)

instance Show IPv6 where show = T.toString
instance ShowT IPv6 where
    toUTF8BuilderP _ ia6@(IPv6 a1 a2 a3 a4)
        -- IPv4-Mapped IPv6 Address
        | a1 == 0 && a2 == 0 && a3 == 0xffff =
            "::ffff:" >> T.toUTF8Builder (IPv4 a4)
        -- IPv4-Compatible IPv6 Address (exclude IPRange ::/112)
        | a1 == 0 && a2 == 0 && a3 == 0 && a4 >= 0x10000 =
            "::" >> T.toUTF8Builder (IPv4 a4)
        -- length of longest run > 1, replace it with "::"
        | end - begin > 1 =
            showFields prefix >> "::" >> showFields suffix
        | otherwise =
            showFields fields
      where
        fields =
            let (u7, u6, u5, u4, u3, u2, u1, u0) = ipv6AddrToTuple ia6 in
            [u7, u6, u5, u4, u3, u2, u1, u0]
        showFields = T.intercalateList (T.char7 ':') (\ f -> T.string7 (showHex f []))
        !prefix = take begin fields  -- fields before "::"
        !suffix = drop end fields    -- fields after "::"
        begin = end + diff          -- the longest run of zeros
        (diff, end) = minimum $
            scanl (\c i -> if i == 0 then c - 1 else 0) 0 fields `zip` [0..]

-- | @::@
ipv6Any      :: IPv6
ipv6Any       = IPv6 0 0 0 0

-- | @::1@
ipv6Loopback :: IPv6
ipv6Loopback  = IPv6 0 0 0 1

-- | convert 'IPv6' to octets.
ipv6AddrToTuple :: IPv6 -> (Word16, Word16, Word16, Word16,
                                        Word16, Word16, Word16, Word16)
ipv6AddrToTuple (IPv6 w3 w2 w1 w0) =
    let high, low :: Word32 -> Word16
        high w = fromIntegral (w `shiftR` 16)
        low w = fromIntegral w
    in (high w3, low w3, high w2, low w2, high w1, low w1, high w0, low w0)

-- | convert 'IPv6' from octets.
tupleToIPv6Addr :: (Word16, Word16, Word16, Word16,
                        Word16, Word16, Word16, Word16) -> IPv6
tupleToIPv6Addr (w7, w6, w5, w4, w3, w2, w1, w0) =
    let add :: Word16 -> Word16 -> Word32
        high `add` low = (fromIntegral high `shiftL` 16) .|. (fromIntegral low)
    in  IPv6 (w7 `add` w6) (w5 `add` w4) (w3 `add` w2) (w1 `add` w0)

instance Storable IPv6 where
    sizeOf _    = #size struct in6_addr
    alignment _ = #alignment struct in6_addr
    peek p = do
        a <- peek32 p 0
        b <- peek32 p 1
        c <- peek32 p 2
        d <- peek32 p 3
        return $ IPv6 a b c d
    poke p (IPv6 a b c d) = do
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

instance Unaligned IPv6 where
    unalignedSize _ = (#size struct in6_addr)

    indexBA p off = 
        let a = indexBA p (off + s6_addr_offset + 0)
            b = indexBA p  (off + s6_addr_offset + 4)
            c = indexBA p  (off + s6_addr_offset + 8)
            d = indexBA p  (off + s6_addr_offset + 12)
        in IPv6 (getBE a) (getBE b) (getBE c) (getBE d)

    peekMBA p off = do
        a <- peekMBA p (off + s6_addr_offset + 0)
        b <- peekMBA p  (off + s6_addr_offset + 4)
        c <- peekMBA p  (off + s6_addr_offset + 8)
        d <- peekMBA p  (off + s6_addr_offset + 12)
        return $ IPv6 (getBE a) (getBE b) (getBE c) (getBE d)

    pokeMBA p off (IPv6 a b c d) = do
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
            return (SocketAddrIPv4 addr port)
        (#const AF_INET6) -> do
            port <- (#peek struct sockaddr_in6, sin6_port) p
            flow <- (#peek struct sockaddr_in6, sin6_flowinfo) p
            addr <- (#peek struct sockaddr_in6, sin6_addr) p
            scope <- (#peek struct sockaddr_in6, sin6_scope_id) p
            return (SocketAddrIPv6 addr port flow scope)
        _ -> do let errno = UV_EAI_ADDRFAMILY
                name <- uvErrName errno
                desc <- uvStdError errno
                throwUVError errno (IOEInfo name desc callStack)

pokeSocketAddr :: Ptr SocketAddr -> SocketAddr -> IO ()
pokeSocketAddr p (SocketAddrIPv4 addr port) =  do
#if defined(darwin_HOST_OS)
    clearPtr p (#size struct sockaddr_in)
#endif
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
    (#poke struct sockaddr_in, sin_len) p ((#size struct sockaddr_in) :: Word8)
#endif
    (#poke struct sockaddr_in, sin_family) p ((#const AF_INET) :: CSaFamily)
    (#poke struct sockaddr_in, sin_port) p port
    (#poke struct sockaddr_in, sin_addr) p addr
pokeSocketAddr p (SocketAddrIPv6 addr port flow scope) =  do
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
withSocketAddr sa@(SocketAddrIPv4 _ _) f = do
    allocaBytesAligned
        (#size struct sockaddr_in)
        (#alignment struct sockaddr_in) $ \ p -> pokeSocketAddr p sa >> f p
withSocketAddr sa@(SocketAddrIPv6 _ _ _ _) f = do
    allocaBytesAligned 
        (#size struct sockaddr_in6) 
        (#alignment struct sockaddr_in6) $ \ p -> pokeSocketAddr p sa >> f p

-- | Pass 'SocketAddr' to FFI as pointer.
--
-- USE THIS FUNCTION WITH UNSAFE FFI CALL ONLY.
--
withSocketAddrUnsafe :: SocketAddr -> (MBA## SocketAddr -> IO a) -> IO a
withSocketAddrUnsafe sa@(SocketAddrIPv4 _ _) f = do
    (MutableByteArray p) <- newByteArray (#size struct sockaddr_in) 
    pokeSocketAddrMBA p sa 
    f p
withSocketAddrUnsafe sa@(SocketAddrIPv6 _ _ _ _) f = do
    (MutableByteArray p) <- newByteArray (#size struct sockaddr_in6) 
    pokeSocketAddrMBA p sa 
    f p

sizeOfSocketAddr :: SocketAddr -> CSize
sizeOfSocketAddr (SocketAddrIPv4 _ _) = #size struct sockaddr_in
sizeOfSocketAddr (SocketAddrIPv6 _ _ _ _) = #size struct sockaddr_in6

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
            return (SocketAddrIPv4 addr port)
        (#const AF_INET6) -> do
            port <- peekMBA p (#offset struct sockaddr_in6, sin6_port) 
            flow <- peekMBA p (#offset struct sockaddr_in6, sin6_flowinfo) 
            addr <- peekMBA p (#offset struct sockaddr_in6, sin6_addr) 
            scope <- peekMBA p (#offset struct sockaddr_in6, sin6_scope_id) 
            return (SocketAddrIPv6 addr port flow scope)
        _ -> do let errno = UV_EAI_ADDRFAMILY
                name <- uvErrName errno
                desc <- uvStdError errno
                throwUVError errno (IOEInfo name desc callStack)

pokeSocketAddrMBA :: MBA## SocketAddr -> SocketAddr -> IO ()
pokeSocketAddrMBA p (SocketAddrIPv4 addr port) =  do
#if defined(darwin_HOST_OS)
    clearMBA p (#size struct sockaddr_in)
#endif
#if defined(HAVE_STRUCT_SOCKADDR_SA_LEN)
    pokeMBA p (#offset struct sockaddr_in, sin_len) ((#size struct sockaddr_in) :: Word8)
#endif
    pokeMBA p (#offset struct sockaddr_in, sin_family) ((#const AF_INET) :: CSaFamily)
    pokeMBA p (#offset struct sockaddr_in, sin_port) port
    pokeMBA p (#offset struct sockaddr_in, sin_addr) addr
pokeSocketAddrMBA p (SocketAddrIPv6 addr port flow scope) =  do
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
-- 
-- Use the @Num@ instance (i.e. use a literal) to create a --   @PortNumber@ value.
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
newtype PortNumber = PortNumber Word16 
    deriving (Eq, Ord, Enum, Generic)
    deriving newtype (Show, ShowT, Read, Num, Bounded, Real, Integral, EncodeJSON, ToValue, FromValue)

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

type SocketFamily = CInt
type SocketType = CInt
type ProtocolNumber = CInt

-- | unspecified
pattern AF_UNSPEC :: SocketFamily
pattern AF_UNSPEC = #const AF_UNSPEC
-- | internetwork: UDP, TCP, etc
pattern AF_INET :: SocketFamily
pattern AF_INET = #const AF_INET
-- | Internet Protocol version 6
pattern AF_INET6 :: SocketFamily
pattern AF_INET6 = #const AF_INET6

pattern SOCK_STREAM :: SocketType
pattern SOCK_STREAM = #const SOCK_STREAM
pattern SOCK_DGRAM :: SocketType
pattern SOCK_DGRAM = #const SOCK_DGRAM
pattern SOCK_RAW :: SocketType
pattern SOCK_RAW = #const SOCK_RAW
pattern SOCK_RDM :: SocketType
pattern SOCK_RDM = #const SOCK_RDM
pattern SOCK_SEQPACKET :: SocketType
pattern SOCK_SEQPACKET = #const SOCK_SEQPACKET
-- | Used in getAddrInfo hints, for any type can be returned by getAddrInfo
pattern SOCK_ANY :: SocketType
pattern SOCK_ANY = 0

pattern IPPROTO_DEFAULT :: ProtocolNumber
pattern IPPROTO_DEFAULT = 0
pattern IPPROTO_IP :: ProtocolNumber
pattern IPPROTO_IP = #const IPPROTO_IP
pattern IPPROTO_TCP :: ProtocolNumber
pattern IPPROTO_TCP =  #const IPPROTO_TCP
pattern IPPROTO_UDP :: ProtocolNumber
pattern IPPROTO_UDP = #const IPPROTO_UDP

--------------------------------------------------------------------------------

foreign import #{CALLCONV} unsafe "ntohs" ntohs :: Word16 -> Word16
foreign import #{CALLCONV} unsafe "htons" htons :: Word16 -> Word16
foreign import #{CALLCONV} unsafe "ntohl" ntohl :: Word32 -> Word32
foreign import #{CALLCONV} unsafe "htonl" htonl :: Word32 -> Word32

foreign import ccall unsafe uv_ip4_addr :: BA## Word8 -> CInt -> MBA## SocketAddr -> IO CInt
foreign import ccall unsafe uv_ip6_addr :: BA## Word8 -> CInt -> MBA## SocketAddr -> IO CInt
