{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
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

* Socket FD is created lazily if no local address is provided, that means various functions
  that need FD will throw bad FD exception if you 'initUDP' with no local address e.g. 'setTTL'.

* If you want to create socket FD but don't care about which port or interface you're using,
  use 'SocketAddrInet' 'portAny' 'inetAny' when 'initUDP'.

* Prefer 'recvUDPLoop' because it can reuse receive buffer.

-}

module Z.IO.Network.UDP (
  -- * TCP Client
    UDP
  , initUDP
  , UDPConfig(..)
  , defaultUDPConfig
  , UDPFlag(UDP_DEFAULT, UDP_IPV6ONLY, UDP_REUSEADDR)
  , sendUDP
  , UDPRecvConfig(..)
  , defaultUDPRecvConfig
  , recvUDPLoop
  , recvUDP
  , getSockName
  -- * Connected UDP Client
  , ConnectedUDP
  , connectUDP
  , disconnectUDP
  , getPeerName
  , sendConnectedUDP
  -- * multicast and broadcast
  , Membership(JOIN_GROUP, LEAVE_GROUP)
  , setMembership
  , setSourceMembership
  , setMulticastLoop
  , setMulticastTTL
  , setMulticastInterface
  , setBroadcast
  , setTTL
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Primitive  (primitive_)
import Data.Primitive.PrimArray as A
import Data.IORef
import Data.Word
import Data.Int
import Data.Bits ((.&.))
import Foreign.Storable (peek, poke)
import Foreign.Ptr (plusPtr)
import Foreign.C
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

instance Show UDP where
    show (UDP hdl slot uvm _ _) =
        "UDP{udpHandle=" ++ show hdl ++
                ",udpSlot=" ++ show slot ++
                ",udpManager=" ++ show uvm ++ "}"

-- | UDP options.
--
-- Though technically message length field in the UDP header is a max of 65535, but large packets
-- could be more likely dropped by routers,
-- usually a packet(IPV4) with a payload <= 508 bytes is considered safe.
data UDPConfig = UDPConfig
    { udpSendMsgSize :: {-# UNPACK #-} !Int         -- ^ maximum size of sending buffer
    , udpLocalAddr :: Maybe (SocketAddr, UDPFlag)   -- ^ do we want bind a local address before receiving & sending?
                                                    --   set to Nothing to let OS pick a random one.
    } deriving (Show, Eq, Ord)

-- | @UDPConfig 512 Nothing@
defaultUDPConfig :: UDPConfig
defaultUDPConfig = UDPConfig 512 Nothing

-- | Initialize a UDP socket.
--
initUDP :: HasCallStack
        => UDPConfig
        -> Resource UDP
initUDP (UDPConfig sbsiz maddr) = initResource
    (do uvm <- getUVManager
        (hdl, slot) <- withUVManager uvm $ \ loop -> do
            hdl <- hs_uv_handle_alloc loop
            slot <- getUVSlot uvm (peekUVHandleData hdl)
            -- clean up
            _ <- tryTakeMVar =<< getBlockMVar uvm slot

            -- init uv struct
            (do throwUVIfMinus_ (uv_udp_init loop hdl)
                ) `onException` hs_uv_handle_free hdl
            return (hdl, slot)

        -- bind the socket if address is available
        -- This is safe without lock UV manager
        forM_ maddr $ \ (addr, flag) ->
            withSocketAddrUnsafe addr $ \ p ->
                throwUVIfMinus_ (uv_udp_bind hdl p flag)

        sbuf <- A.newPinnedPrimArray (max 0 sbsiz)
        closed <- newIORef False
        return (UDP hdl slot uvm sbuf closed))
    (\ (UDP hdl _ uvm _  closed) -> withUVManager' uvm $ do
        c <- readIORef closed
        -- hs_uv_handle_close won't return error
        unless c $ writeIORef closed True >> hs_uv_handle_close hdl)

checkUDPClosed :: HasCallStack => UDP -> IO ()
checkUDPClosed udp = do
    c <- readIORef (udpClosed udp)
    when c throwECLOSED

-- | Get the local IP and port of the 'UDP'.
getSockName :: HasCallStack => UDP -> IO SocketAddr
getSockName udp@(UDP hdl _ _ _ _) = do
    checkUDPClosed udp
    withSocketAddrStorageUnsafe $ \ paddr ->
        void $ withPrimUnsafe (fromIntegral sizeOfSocketAddrStorage :: CInt) $ \ plen ->
            throwUVIfMinus_ (uv_udp_getsockname hdl paddr plen)

-- | Wrapper for a connected 'UDP'.
newtype ConnectedUDP = ConnectedUDP UDP deriving Show

-- | Associate the UDP handle to a remote address and port,
-- so every message sent by this handle is automatically sent to that destination
connectUDP :: HasCallStack => UDP -> SocketAddr -> IO ConnectedUDP
connectUDP udp@(UDP hdl _ _ _ _) addr = do
    checkUDPClosed udp
    withSocketAddrUnsafe addr $ \ paddr ->
        throwUVIfMinus_ (uv_udp_connect hdl paddr)
    return (ConnectedUDP udp)

-- | Disconnect the UDP handle from a remote address and port.
disconnectUDP :: HasCallStack => ConnectedUDP -> IO UDP
disconnectUDP (ConnectedUDP udp@(UDP hdl _ _ _ _)) = do
    checkUDPClosed udp
    throwUVIfMinus_ (uv_udp_disconnect hdl nullPtr)
    return udp

-- | Get the remote IP and port on 'ConnectedUDP'.
getPeerName :: HasCallStack => ConnectedUDP -> IO SocketAddr
getPeerName (ConnectedUDP udp@(UDP hdl _ _ _ _)) = do
    checkUDPClosed udp
    withSocketAddrStorageUnsafe $ \ paddr ->
        void $ withPrimUnsafe (fromIntegral sizeOfSocketAddrStorage :: CInt) $ \ plen ->
            throwUVIfMinus_ (uv_udp_getpeername hdl paddr plen)

-- | Send a UDP message with a connected UDP.
--
-- WARNING: A 'InvalidArgument' with errno 'UV_EMSGSIZE' will be thrown
-- if message is larger than 'sendMsgSize'.
sendConnectedUDP :: HasCallStack => ConnectedUDP -> V.Bytes -> IO ()
sendConnectedUDP (ConnectedUDP udp@(UDP hdl _ uvm sbuf _)) (V.PrimVector ba s la) = mask_ $ do
    checkUDPClosed udp
    -- copy message to pinned buffer
    lb <- getSizeofMutablePrimArray sbuf
    when (la > lb) (throwUVIfMinus_ (return UV_EMSGSIZE))
    copyPrimArray sbuf 0 ba s la
    withMutablePrimArrayContents sbuf $ \ pbuf -> do
        m <- withUVManager' uvm $ do
            reqSlot <- getUVSlot uvm (hs_uv_udp_send_connected hdl pbuf la)
            reqMVar <- getBlockMVar uvm reqSlot
            -- since we locked uv manager here, it won't affect next event
            _ <- tryTakeMVar reqMVar
            return reqMVar
        -- we can't cancel uv_udp_send_t in current libuv
        -- and disaster will happen if buffer got collected.
        -- so we have to turn to uninterruptibleMask_'s help.
        -- i.e. sendUDP is an uninterruptible operation.
        -- OS will guarantee writing a socket will not
        -- hang forever anyway.
        throwUVIfMinus_  (uninterruptibleMask_ $ takeMVar m)

-- | Send a UDP message to target address.
--
-- WARNING: A 'InvalidArgument' with errno 'UV_EMSGSIZE' will be thrown
-- if message is larger than 'sendMsgSize'.
sendUDP :: HasCallStack => UDP -> SocketAddr -> V.Bytes -> IO ()
sendUDP udp@(UDP hdl _ uvm sbuf _) addr (V.PrimVector ba s la) = mask_ $ do
    checkUDPClosed udp
    -- copy message to pinned buffer
    lb <- getSizeofMutablePrimArray sbuf
    when (la > lb) (throwUVIfMinus_ (return UV_EMSGSIZE))
    copyPrimArray sbuf 0 ba s la
    withMutablePrimArrayContents sbuf $ \ pbuf -> do
        m <- withUVManager' uvm $ do
            reqSlot <- withSocketAddrUnsafe addr $ \ paddr ->
                getUVSlot uvm (hs_uv_udp_send hdl paddr pbuf la)
            reqMVar <- getBlockMVar uvm reqSlot
            -- since we locked uv manager here, it won't affect next event
            _ <- tryTakeMVar reqMVar
            return reqMVar
        -- we can't cancel uv_udp_send_t in current libuv
        -- and disaster will happen if buffer got collected.
        -- so we have to turn to uninterruptibleMask_'s help.
        -- i.e. sendUDP is an uninterruptible operation.
        -- OS will guarantee writing a socket will not
        -- hang forever anyway.
        throwUVIfMinus_  (uninterruptibleMask_ $ takeMVar m)

-- | Set IP multicast loop flag. Makes multicast packets loop back to local sockets.
setMulticastLoop :: HasCallStack => UDP -> Bool -> IO ()
setMulticastLoop udp@(UDP hdl _ _ _ _) loop = do
    checkUDPClosed udp
    throwUVIfMinus_ (uv_udp_set_multicast_loop hdl (if loop then 1 else 0))

-- | Set the multicast ttl.
setMulticastTTL :: HasCallStack => UDP -> Int -> IO ()
setMulticastTTL udp@(UDP hdl _ _ _ _) ttl = do
    checkUDPClosed udp
    throwUVIfMinus_ (uv_udp_set_multicast_ttl hdl (fromIntegral ttl'))
  where ttl' = V.rangeCut ttl 1 255

-- | Set the multicast interface to send or receive data on.
setMulticastInterface :: HasCallStack => UDP -> CBytes ->IO ()
setMulticastInterface udp@(UDP hdl _ _ _ _) iaddr = do
    checkUDPClosed udp
    withCBytesUnsafe iaddr $ \ iaddrp ->
        throwUVIfMinus_ (uv_udp_set_multicast_interface hdl iaddrp)

-- | Set broadcast on or off.
setBroadcast :: HasCallStack => UDP -> Bool -> IO ()
setBroadcast udp@(UDP hdl _ _ _ _) b = do
    checkUDPClosed udp
    throwUVIfMinus_ (uv_udp_set_broadcast hdl (if b then 1 else 0))

-- | Set the time to live.
setTTL :: HasCallStack
       => UDP
       -> Int       -- ^ 1 ~ 255
       -> IO ()
setTTL udp@(UDP hdl _ _ _ _) ttl = do
    checkUDPClosed udp
    throwUVIfMinus_ (uv_udp_set_ttl hdl (fromIntegral ttl))

-- | Set membership for a multicast group.
setMembership :: HasCallStack
              => UDP
              -> CBytes             -- ^ Multicast address to set membership for.
              -> CBytes             -- ^ Interface address.
              -> Membership       -- ^ UV_JOIN_GROUP | UV_LEAVE_GROUP
              -> IO ()
setMembership udp@(UDP hdl _ _ _ _) gaddr iaddr member = do
    checkUDPClosed udp
    withCBytesUnsafe gaddr $ \ gaddrp ->
        withCBytesUnsafe iaddr $ \ iaddrp ->
            throwUVIfMinus_ (uv_udp_set_membership hdl gaddrp iaddrp member)

-- | Set membership for a source-specific multicast group.
setSourceMembership :: HasCallStack
                    => UDP
                    -> CBytes           -- ^ Multicast address to set membership for.
                    -> CBytes           -- ^ Interface address.
                    -> CBytes           -- ^ Source address.
                    -> Membership     -- ^ UV_JOIN_GROUP | UV_LEAVE_GROUP
                    -> IO ()
setSourceMembership udp@(UDP hdl _ _ _ _) gaddr iaddr source member = do
    checkUDPClosed udp
    withCBytesUnsafe gaddr $ \ gaddrp ->
        withCBytesUnsafe iaddr $ \ iaddrp ->
            withCBytesUnsafe source $ \ sourcep ->
                throwUVIfMinus_ (uv_udp_set_source_membership hdl gaddrp iaddrp sourcep member)

--------------------------------------------------------------------------------

-- | Receiving buffering config.
--
data UDPRecvConfig = UDPRecvConfig
    { recvMsgSize :: {-# UNPACK #-} !Int32      -- ^ maximum size of a received message
    , recvBatchSize :: {-# UNPACK #-} !Int      -- ^ how many messages we want to receive per uv loop,
                                                --   inside each uv_run, we do batch receiving,
                                                --   increase this number can improve receiving performance,
                                                --   at the cost of memory and potential GHC thread starving.
    }

-- | @UDPRecvConfig 512 6@
defaultUDPRecvConfig :: UDPRecvConfig
defaultUDPRecvConfig = UDPRecvConfig 512 6


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

-- | Recv UDP message within a loop
--
-- Loop receiving can be faster since it can reuse receiving buffer.
recvUDPLoop :: HasCallStack
            => UDPRecvConfig
            -> UDP
            -> ((Maybe SocketAddr, Bool, V.Bytes) -> IO a)
            -> IO ()
recvUDPLoop (UDPRecvConfig bufSiz bufArrSiz) udp@(UDP hdl slot uvm _ _) worker = do
    bracket
        (throwOOMIfNull $ hs_uv_udp_check_alloc hdl)
        hs_uv_udp_check_close $
        \ check -> do
            buf@(_, rbufArr) <- newRecvBuf bufSiz bufArrSiz
            withMutablePrimArrayContents rbufArr $ \ p -> do
                pokeBufferTable uvm slot (castPtr p) (bufArrSiz-1)
                -- init uv_check_t must come after poking buffer
                throwUVIfMinus_ $ hs_uv_udp_check_init check
            forever $ do
                msgs <- recvUDPWith udp check buf bufSiz
                withMutablePrimArrayContents rbufArr $ \ p ->
                    pokeBufferTable uvm slot (castPtr p) (bufArrSiz-1)
                forM_ msgs worker

-- | Recv messages from UDP socket, return source address if available, and a `Bool`
-- to indicate if the message is partial (larger than receive buffer size).
--
recvUDP :: HasCallStack => UDPRecvConfig -> UDP -> IO [(Maybe SocketAddr, Bool, V.Bytes)]
recvUDP (UDPRecvConfig bufSiz bufArrSiz) udp@(UDP hdl slot uvm _ _)  = do
    bracket
        (throwOOMIfNull $ hs_uv_udp_check_alloc hdl)
        hs_uv_udp_check_close $
        \ check -> do
            buf@(_, rbufArr) <- newRecvBuf bufSiz bufArrSiz
            withMutablePrimArrayContents rbufArr $ \ p -> do
                pokeBufferTable uvm slot (castPtr p) (bufArrSiz-1)
                -- init uv_check_t must come after poking buffer
                throwUVIfMinus_ $ hs_uv_udp_check_init check
            recvUDPWith udp check buf bufSiz

recvUDPWith :: HasCallStack
            => UDP
            -> Ptr UVHandle -- ^ uv_check_t
            -> (A.MutablePrimArray RealWorld Word8, A.MutablePrimArray RealWorld (Ptr Word8))
            -> Int32
            -> IO [(Maybe SocketAddr, Bool, V.Bytes)]
recvUDPWith udp@(UDP hdl slot uvm _ _) check (rubf, rbufArr) bufSiz =
    -- It's important to keep recv buffer alive, even if we don't directly use it
    mask_ . withMutablePrimArrayContents rubf $ \ _ -> do
        checkUDPClosed udp

        bufArrSiz <- getSizeofMutablePrimArray rbufArr
        -- we have to reset the buffer size, during receiving it'll be overwritten
        forM_ [0..bufArrSiz-1] $ \ i -> do
            p <- readPrimArray rbufArr i
            poke (castPtr p :: Ptr Int32) bufSiz

        m <- getBlockMVar uvm slot

        throwUVIfMinus_ . withUVManager' uvm $ do
            -- clean up
            _ <- tryTakeMVar m
            hs_uv_udp_recv_start hdl

        r <- takeMVar m `onException` (do
                -- normally we call 'uv_udp_recv_stop' in C read callback
                -- but when exception raise, here's the place to stop
                throwUVIfMinus_ $ withUVManager' uvm (uv_udp_recv_stop hdl)
                void (tryTakeMVar m))

        forM [r+1..bufArrSiz-1] $ \ i -> do
            p        <- readPrimArray rbufArr i
            -- see the buffer struct diagram above
            result   <- throwUVIfMinus (fromIntegral <$> peek @Int32 (castPtr p))
            flag     <- peek @Int32 (castPtr (p `plusPtr` 4))
            addrFlag <- peek @Int32 (castPtr (p `plusPtr` 8))
            !addr <- if addrFlag == 1
                then Just <$!> peekSocketAddr (castPtr (p `plusPtr` 12))
                else return Nothing
            let !partial = flag .&. UV_UDP_PARTIAL /= 0
            mba <- A.newPrimArray result
            copyPtrToMutablePrimArray mba 0 (p `plusPtr` 140) result
            ba <- A.unsafeFreezePrimArray mba
            return (addr, partial, V.PrimVector ba 0 result)
