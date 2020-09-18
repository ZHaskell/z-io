{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : Z.IO.Network.UDP
Description : UDP servers and clients
Copyright   : (c) Dong Han, 2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides an API for creating UDP sender and receiver.

-}

module Z.IO.Network.UDP (
  -- * TCP Client
    UDP(..)
  , initUDP
  , UDPConfig(..)
  , defaultUDPConfig
  , UVUDPFlag(UV_UDP_DEFAULT, UV_UDP_IPV6ONLY, UV_UDP_REUSEADDR)
  , sendUDP
  , recvUDPLoop
  , recvUDP
  , getUDPSockName
  -- * multicast and broadcast
  , UVMembership(UV_JOIN_GROUP, UV_LEAVE_GROUP)
  , setMembership
  , setSourceMembership
  , setMulticastLoop
  , setMulticastTTL
  , setMulticastInterface
  , setBroadcast
  , setTTL
  ) where

import Control.Monad.Primitive  (primitive_)
import Data.Primitive.PrimArray as A
import Data.Primitive.Ptr       (copyPtrToMutablePrimArray)
import Data.IORef
import GHC.Prim                 (touch#)
import Z.Data.Array           as A
import Z.Data.Vector.Base     as V
import Z.Data.Vector.Extra    as V
import Z.Data.CBytes          as CBytes
import Z.IO.Network.SocketAddr
import Z.Foreign
import Z.IO.UV.Errno          (pattern UV_EMSGSIZE)
import Z.IO.UV.FFI
import Z.IO.UV.Manager
import Z.IO.Exception
import Z.IO.Resource
import Data.Word
import Data.Int
import Data.Bits ((.&.))
import Control.Monad
import Control.Concurrent.MVar
import Foreign.Storable (peek, poke)
import Foreign.Ptr (plusPtr)
import Foreign.C

-- | UDP socket client.
--
-- UDP is not a sequential protocol, thus not an instance of 'Input/Output'.
-- Message are received or sent individually, UDP socket client is NOT thread safe!
-- Use 'MVar' 'UDP' in multiple threads.
--
data UDP = UDP
    { udpHandle  :: {-# UNPACK #-} !(Ptr UVHandle)
    , udpSlot    :: {-# UNPACK #-} !UVSlot
    , udpManager :: UVManager
    , udpSendBuffer ::  {-# UNPACK #-} !(A.MutablePrimArray RealWorld Word8)
    , udpClosed  :: {-# UNPACK #-} !(IORef Bool)
    }

-- | UDP options.
--
-- Though technically message length field in the UDP header is a max of 65535, but large packets
-- could be more likely dropped by routers, usually a packet(IPV4) with a payload <= 508 bytes is considered safe.
data UDPConfig = UDPConfig
    { updSendMsgSize :: {-# UNPACK #-} !Int           -- ^ maximum size of sending buffer
    , udpLocalAddr :: Maybe (SocketAddr, UVUDPFlag) -- ^ do we want bind a local address before receiving & sending?
                                                   --   set to Nothing to let OS pick a random one.
    } deriving (Show, Eq, Ord)

-- | default 'UDPConfig', @defaultUDPConfig = UDPConfig 512 6 512 Nothing@
defaultUDPConfig = UDPConfig 512 Nothing

-- | Initialize a UDP socket.
--
initUDP :: HasCallStack
              => UDPConfig
              -> Resource UDP
initUDP (UDPConfig sbsiz maddr) = initResource
    (do uvm <- getUVManager
        (handle, slot) <- withUVManager uvm $ \ loop -> do
            handle <- hs_uv_handle_alloc loop
            slot <- getUVSlot uvm (peekUVHandleData handle)
            tryTakeMVar =<< getBlockMVar uvm slot  -- clear the parking spot

            -- init uv struct
            (do throwUVIfMinus_ (uv_udp_init loop handle)
                -- bind the socket if address is available
                forM_ maddr $ \ (addr, flag) ->
                    withSocketAddr addr $ \ p ->
                        throwUVIfMinus_ (uv_udp_bind handle p flag)
                ) `onException` hs_uv_handle_free handle
            return (handle, slot)

        sbuf <- A.newPinnedPrimArray (max 0 sbsiz)
        closed <- newIORef False
        return (UDP handle slot uvm sbuf closed))
    closeUDP

closeUDP :: UDP -> IO ()
closeUDP (UDP handle _ uvm _  closed) = withUVManager' uvm $ do
    c <- readIORef closed
    -- hs_uv_handle_close won't return error
    unless c $ writeIORef closed True >> hs_uv_handle_close handle

checkUDPClosed :: HasCallStack => UDP -> IO ()
checkUDPClosed udp = do
    c <- readIORef (udpClosed udp)
    when c throwECLOSED

getUDPSockName :: HasCallStack => UDP -> IO SocketAddr
getUDPSockName udp@(UDP handle _ _ _ _) = do
    checkUDPClosed udp
    withSocketAddrStorage $ \ paddr ->
        void $ withPrimUnsafe (fromIntegral sizeOfSocketAddrStorage :: CInt) $ \ plen ->
            throwUVIfMinus_ (uv_udp_getsockname handle paddr plen)

-- | Send a UDP message to target address.
--
-- WARNING: A 'InvalidArgument' with errno 'UV_EMSGSIZE' will be thrown
-- if message is larger than 'sendMsgSize'.
sendUDP :: HasCallStack => UDP -> SocketAddr -> V.Bytes -> IO ()
sendUDP udp@(UDP handle slot uvm sbuf closed) addr (V.PrimVector ba s la) = mask_ $ do
    checkUDPClosed udp
    -- copy message to pinned buffer
    lb <- getSizeofMutablePrimArray sbuf
    when (la > lb) (throwUVIfMinus_ (return UV_EMSGSIZE))
    copyPrimArray sbuf 0 ba s la
    withSocketAddr addr $ \ paddr ->
        withMutablePrimArrayContents sbuf $ \ pbuf -> do
            (slot, m) <- withUVManager' uvm $ do
                slot <- getUVSlot uvm (hs_uv_udp_send handle paddr pbuf la)
                m <- getBlockMVar uvm slot
                tryTakeMVar m
                return (slot, m)
            -- we can't cancel uv_udp_send_t in current libuv
            -- and disaster will happen if buffer got collected.
            -- so we have to turn to uninterruptibleMask_'s help.
            -- i.e. sendUDP is an uninterruptible operation.
            -- OS will guarantee writing a socket will not
            -- hang forever anyway.
            throwUVIfMinus_  (uninterruptibleMask_ $ takeMVar m)

setMulticastLoop :: HasCallStack => UDP -> Bool -> IO ()
setMulticastLoop udp@(UDP handle _ _ _ _) loop = do
    checkUDPClosed udp
    throwUVIfMinus_ (uv_udp_set_multicast_loop handle (if loop then 1 else 0))

setMulticastTTL :: HasCallStack => UDP -> Int -> IO ()
setMulticastTTL udp@(UDP handle _ _ _ _) ttl = do
    checkUDPClosed udp
    throwUVIfMinus_ (uv_udp_set_multicast_ttl handle (fromIntegral ttl'))
  where ttl' = V.rangeCut ttl 1 255

setMulticastInterface :: HasCallStack => UDP -> CBytes ->IO ()
setMulticastInterface udp@(UDP handle _ _ _ _) iaddr = do
    checkUDPClosed udp
    withCBytes iaddr $ \ iaddrp ->
        throwUVIfMinus_ (uv_udp_set_multicast_interface handle iaddrp)

setBroadcast :: HasCallStack => UDP -> Bool -> IO ()
setBroadcast udp@(UDP handle _ _ _ _) b = do
    checkUDPClosed udp
    throwUVIfMinus_ (uv_udp_set_broadcast handle (if b then 1 else 0))

setTTL :: HasCallStack
       => UDP
       -> Int       -- ^ 1 ~ 255
       -> IO ()
setTTL udp@(UDP handle _ _ _ _) ttl = do
    checkUDPClosed udp
    throwUVIfMinus_ (uv_udp_set_ttl handle (fromIntegral ttl))


setMembership :: HasCallStack
              => UDP
              -> CBytes             -- ^ Multicast address to set membership for.
              -> CBytes             -- ^ Interface address.
              -> UVMembership       -- ^ UV_JOIN_GROUP | UV_LEAVE_GROUP
              -> IO ()
setMembership udp@(UDP handle _ _ _ _) gaddr iaddr member = do
    checkUDPClosed udp
    withCBytes gaddr $ \ gaddrp ->
        withCBytes iaddr $ \ iaddrp ->
            throwUVIfMinus_ (uv_udp_set_membership handle gaddrp iaddrp member)

setSourceMembership :: HasCallStack
                    => UDP
                    -> CBytes           -- ^ Multicast address to set membership for.
                    -> CBytes           -- ^ Interface address.
                    -> CBytes           -- ^ Source address.
                    -> UVMembership     -- ^ UV_JOIN_GROUP | UV_LEAVE_GROUP
                    -> IO ()
setSourceMembership udp@(UDP handle _ _ _ _) gaddr iaddr source member = do
    checkUDPClosed udp
    withCBytes gaddr $ \ gaddrp ->
        withCBytes iaddr $ \ iaddrp ->
            withCBytes source $ \ sourcep ->
                throwUVIfMinus_ (uv_udp_set_source_membership handle gaddrp iaddrp sourcep member)

--------------------------------------------------------------------------------

data UDPRecvConfig = UDPRecvConfig
    { recvMsgSize :: {-# UNPACK #-} !Int32      -- ^ maximum size of a received message
    , recvBatchSize :: {-# UNPACK #-} !Int      -- ^ how many messages we want to receive per uv loop,
                                                --   inside each uv_run, we do batch receiving,
                                                --   increase this number can improve receiving performance,
                                                --   at the cost of memory and potential GHC thread starving.
    }


-- The buffer passing of UDP is a litte complicated here, to get maximum performance,
-- we do batch receiving. i.e. recv multiple messages inside libuv's event loop:
--
--   udpRecvLargeBuffer:
--
--   +---------+--------------+-----------+----------+--------+---------+------------
--   | buf siz | partial flag | addr flag |   addr   | buffer | buf siz | partial ...
--   +--4bytes-+----4bytes----+--4bytes---+-128bytes-+-bufsiz-+---------+------------
--   ^                                                        ^
--   |                                                        |
--   +---------------------+       +--------------------------+
--                         |       |
--                      +--+---+---+--+----
--   udpRecvBufferArray | buf0 | buf1 | ...
--                      +------+------+----
--
-- We allocate a large buffer (buffer_size * buffer_number),
-- each time we poke the udpRecvBufferArray and its last index (size - 1) to uv manager's buffer table.
--
-- On libuv side each alloc callback picks the last pointer from udpRecvBufferArray, decrease last index by 1
-- the read result is write into the `buf siz` cell, then followed with partial flag, if addr is not NULL
-- then addr flag is 1 (otherwise 0), following addr if not NULL, the buffer is already written when
-- recv callback is called.
--
-- On haskell side, we read buffer table's size, which is decreased by n(which is the times callback are called).
-- Then we poke those cells out.
--
newRecvBuf :: Int32 -> Int -> IO (A.MutablePrimArray RealWorld Word8, A.MutablePrimArray RealWorld (Ptr Word8))
newRecvBuf bufSiz bufArrSiz = do
    rbuf <- A.newPinnedPrimArray (fromIntegral bufsiz' * bufArrSiz')
    rbufArr <- A.newPinnedPrimArray bufArrSiz'

    -- initialize buffer array with right index
    withMutablePrimArrayContents rbuf $ \ p ->
        forM_ [0..bufArrSiz'-1] $ \ i -> do
            let bufNPtr = p `plusPtr` (i * fromIntegral bufsiz')
            writePrimArray rbufArr i bufNPtr
    return (rbuf, rbufArr)
  where
    -- (message size + sockaddr flag + + flag size) + sockaddr_in size + buffer
    -- see diagram above
    bufsiz' = 140 + (max 0 bufSiz)
    bufArrSiz' = max 1 bufArrSiz


recvUDPLoop :: HasCallStack
            => UDP
            -> UDPRecvConfig
            -> ((Maybe SocketAddr, Bool, V.Bytes) -> IO a)
            -> IO ()
recvUDPLoop udp (UDPRecvConfig bufSiz bufArrSiz) worker = do
    buf <- newRecvBuf bufSiz bufArrSiz
    forever $ do
        msgs <- recvUDPWith udp buf bufSiz
        forM_ msgs worker

-- | Recv messages from UDP socket, return source address if available, and a `Bool`
-- to indicate if the message is partial (larger than receive buffer size).
recvUDP :: HasCallStack => UDP -> UDPRecvConfig -> IO [(Maybe SocketAddr, Bool, V.Bytes)]
recvUDP udp (UDPRecvConfig bufSiz bufArrSiz)  = do
    buf <- newRecvBuf bufSiz bufArrSiz
    recvUDPWith udp buf bufSiz

recvUDPWith :: HasCallStack
            => UDP
            -> (A.MutablePrimArray RealWorld Word8, A.MutablePrimArray RealWorld (Ptr Word8))
            -> Int32
            -> IO [(Maybe SocketAddr, Bool, V.Bytes)]
recvUDPWith udp@(UDP handle slot uvm _ _) ((MutablePrimArray mba#), rbufArr) bufSiz = mask_ $ do
    checkUDPClosed udp
    m <- getBlockMVar uvm slot
    rbufArrSiz <- getSizeofMutablePrimArray rbufArr

    -- we have to reset the buffer size, during receiving it'll be overwritten
    forM_ [0..rbufArrSiz-1] $ \ i -> do
        p <- readPrimArray rbufArr i
        poke (castPtr p :: Ptr Int32) bufSiz

    -- reset buffer table's size with buffer array's length, during receiving it'll be decreased
    withMutablePrimArrayContents rbufArr $ \ p ->
        pokeBufferTable uvm slot (castPtr p) rbufArrSiz

    withUVManager' uvm $ do
        throwUVIfMinus_ (hs_uv_udp_recv_start handle)
        tryTakeMVar m

    r <- catch (takeMVar m) (\ (e :: SomeException) -> do
            withUVManager' uvm (uv_udp_recv_stop handle)
            -- after we locked uvm and stop reading, the reading probably finished
            -- so try again
            r <- tryTakeMVar m
            case r of Just r -> return r
                      _      -> throwIO e)
    if r < rbufArrSiz
    then forM [rbufArrSiz-1, rbufArrSiz-2 .. r] $ \ i -> do
        p        <- readPrimArray rbufArr i
        -- see the buffer struct diagram above
        result   <- throwUVIfMinus (fromIntegral <$> peek @Int32 (castPtr p))
        flag     <- peek @Int32 (castPtr (p `plusPtr` 4))
        addrFlag <- peek @Int32 (castPtr (p `plusPtr` 8))
        !addr <- if addrFlag == 1
            then Just <$> peekSocketAddr (castPtr (p `plusPtr` 12))
            else return Nothing
        let !partial = flag .&. UV_UDP_PARTIAL /= 0
        mba <- A.newPrimArray result
        copyPtrToMutablePrimArray mba 0 (p `plusPtr` 140) result
        ba <- A.unsafeFreezePrimArray mba
        -- It's important to keep recv buffer alive
        primitive_ (touch# mba#)
        return (addr, partial, V.PrimVector ba 0 result)
    else return []
