{-|
Module      : Z.IO.UV.Manager
Description : IO manager based on libuv
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provide IO manager which bridge libuv's async interface with ghc's light weight thread.

The main procedures for doing event IO is:

  * Allocate uv_handle in C side, get its slot number with 'getUVSlot', or allocate uv_request with 'withUVRequest'.
  * Prepare you IO buffer with 'pokeBufferTable'(read or write).
  * Call C side IO functions with predefined callbacks.
  * Block your thread with the 'MVar' from 'getBlockMVar'.
  * Read the result by 'takeMVar' on that 'MVar', it will be the value pushed on C side.
  * Slot is freed on C side, either via callbacks, or when handle is closed.

Usually slots are cache in the IO device so that you don't have to allocate new one before each IO operation.
Check "Z.IO.Network.TCP" as an example.

-}

module Z.IO.UV.Manager
  ( UVManager
  , getUVManager
  , getBlockMVar
  , peekBufferTable
  , pokeBufferTable
  , withUVManager
  , withUVManager'
  , getUVSlot
  -- * request based async function helper
  , withUVRequest
  , withUVRequest_
  , withUVRequest'
  , withUVRequestEx
  -- * concurrent helpers
  , forkBa
  ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Bits (shiftL)
import           Data.Word
import           GHC.Ptr
import           Foreign.Storable
import           GHC.Conc.Sync            (labelThread)
import           System.IO.Unsafe
import           Z.Data.Array
import           Z.Data.PrimRef.PrimIORef
import           Z.IO.Exception
import           Z.IO.Resource
import           Z.IO.UV.FFI

#define IDLE_LIMIT 20

--------------------------------------------------------------------------------

data UVManager = UVManager
    { uvmBlockTable :: {-# UNPACK #-} !(IORef (UnliftedArray (MVar Int))) -- a array to store threads blocked on async IO.

    , uvmLoop       :: {-# UNPACK #-} !(Ptr UVLoop)        -- the uv loop refrerence

    , uvmLoopData   :: {-# UNPACK #-} !(Ptr UVLoopData)    -- cached pointer to uv_loop_t's data field

    , uvmRunning    :: {-# UNPACK #-} !(MVar Bool)     -- only uv manager thread will modify this value.
                                                        -- 'True' druing uv_run and 'False' otherwise.
                                                        --
                                                        -- unlike epoll/ONESHOT, uv loop are NOT thread safe,
                                                        -- we have to wake up the loop before mutating uv_loop's
                                                        -- state.
    , uvmCap        ::  {-# UNPACK #-} !Int                -- the capability uv manager run on.
    }

instance Show UVManager where
    show uvm = "UVManager on capability " ++ show (uvmCap uvm)

instance Eq UVManager where
    uvm == uvm' =
        uvmCap uvm == uvmCap uvm'

uvManagerArray :: IORef (Array UVManager)
{-# NOINLINE uvManagerArray #-}
uvManagerArray = unsafePerformIO $ do
    numCaps <- getNumCapabilities
    uvmArray <- newArr numCaps
    s <- newQSemN 0
    forM_ [0..numCaps-1] $ \ i -> do
        -- fork uv manager thread
        forkOn i . withResource (initUVManager INIT_LOOP_SIZE i) $ \ m -> do
            myThreadId >>= (`labelThread` ("uv manager on " ++ show i))
            writeArr uvmArray i m
            signalQSemN s 1
            startUVManager m
    waitQSemN s numCaps
    iuvmArray <- unsafeFreezeArr uvmArray
    newIORef iuvmArray

-- | Get 'UVManager' runing on the same capability.
--
getUVManager :: IO UVManager
{-# INLINABLE getUVManager #-}
getUVManager = do
    (cap, _) <- threadCapability =<< myThreadId
    uvmArray <- readIORef uvManagerArray
    indexArrM uvmArray (cap `rem` sizeofArr uvmArray)

-- | Get 'MVar' from blocking table with given slot.
--
getBlockMVar :: UVManager -> UVSlot -> IO (MVar Int)
{-# INLINABLE getBlockMVar #-}
getBlockMVar uvm slot = do
    blockTable <- readIORef (uvmBlockTable uvm)
    indexArrM blockTable slot

-- | Poke a prepared buffer and size into loop data under given slot.
--
-- NOTE, this action is not protected with 'withUVManager' for effcient reason, you should merge this action
-- with other uv action and put them together inside a 'withUVManager' or 'withUVManager\''. for example:
--
-- @
--    ...
--    withUVManager' uvm $ do
--        pokeBufferTable uvm slot buf len
--        uvReadStart handle
--    ...
-- @
--
pokeBufferTable :: UVManager    -- ^ uv manager
                -> UVSlot       -- ^ uv slot
                -> Ptr Word8    -- ^ buffer pointer
                -> Int          -- ^ buffer length
                -> IO ()
{-# INLINABLE pokeBufferTable #-}
pokeBufferTable uvm slot buf bufSiz = do
    (bufTable, bufSizTable) <- peekUVBufferTable (uvmLoopData uvm)
    pokeElemOff bufTable slot buf
    pokeElemOff bufSizTable slot (fromIntegral bufSiz)

peekBufferTable :: UVManager -> UVSlot -> IO Int
{-# INLINABLE peekBufferTable #-}
peekBufferTable uvm slot = do
    (_, bufSizTable) <- peekUVBufferTable (uvmLoopData uvm)
    fromIntegral <$> peekElemOff bufSizTable slot

initUVManager :: HasCallStack => Int -> Int -> Resource UVManager
initUVManager siz cap = do
    loop  <- initResource
                (throwOOMIfNull $ hs_uv_loop_init (fromIntegral siz))
                hs_uv_loop_close
    liftIO $ do
        mblockTable <- newArr siz
        forM_ [0..siz-1] $ \ i -> writeArr mblockTable i =<< newEmptyMVar
        blockTable <- unsafeFreezeArr mblockTable
        blockTableRef <- newIORef blockTable
        loopData <- peekUVLoopData loop
        runningLock <- newMVar False
        return (UVManager blockTableRef loop loopData runningLock cap)

-- | Lock an uv mananger, so that we can safely mutate its uv_loop's state.
--
-- libuv is not thread safe, use this function to perform any action which will mutate uv_loop's state.
--
withUVManager :: HasCallStack => UVManager -> (Ptr UVLoop -> IO a) -> IO a
withUVManager (UVManager _ loop loopData runningLock _) f = go
  where
    go = do
        r <- withMVar runningLock $ \ running -> do
            if running
            then do
                -- if uv_run is running, it will stop
                -- if uv_run is not running, next running won't block
                throwUVIfMinus_ (hs_uv_wake_up_async loopData)
                return Nothing
            else do
                r <- f loop
                return (Just r)
        case r of
            Just r' -> return r'
            _       -> yield >> go -- we yield here, because uv_run is probably not finished yet

-- | Lock an uv mananger, so that we can safely mutate its uv_loop's state.
--
-- Some action did not request uv_loop pointer explicitly, but will mutate uv_loop underhood, for example:
-- @uv_read_start@. These actions have to be protected by locking the uv_loop.
--
-- In fact most of the libuv's functions are not thread safe, so watch out!
--
withUVManager' :: HasCallStack => UVManager -> IO a -> IO a
withUVManager' uvm f = withUVManager uvm (\ _ -> f)

-- | Start the uv loop
--
startUVManager :: HasCallStack => UVManager -> IO ()
startUVManager uvm@(UVManager _ _ _ runningLock _) = poll -- use a closure capture uvm in case of stack memory leaking
  where
    -- we borrow mio's non-blocking/blocking poll strategy here
    poll = do
        e <- withMVar runningLock $ \ _ -> step uvm False
        if e > 0                                        -- first we do a non-blocking poll, if we got events
        then yield >> poll                              -- we yield here, to let other threads do actual work
        else do                                         -- otherwise we still yield once
            yield                                       -- in case other threads can still progress
            e' <- withMVar runningLock $ \ _ -> step uvm False   -- now we do another non-blocking poll to make sure
            if e' > 0 then yield >> poll             -- if we got events somehow, we yield and go back
            else do                                 -- if there's still no events, we directly jump to safe blocking poll
                _ <- swapMVar runningLock True          -- after swap this lock, other thread can wake up us
                _ <- step uvm True                  -- by send async handler, and it's thread safe
                _ <- swapMVar runningLock False

                yield                               -- we yield here, to let other threads do actual work
                poll

    -- call uv_run, return the event number
    step :: UVManager -> Bool -> IO Int
    step (UVManager blockTableRef loop loopData _ _) block = do
            blockTable <- readIORef blockTableRef
            clearUVEventCounter loopData        -- clean event counter

            if block
            then if rtsSupportsBoundThreads
                then throwUVIfMinus_ $ uv_run_safe loop UV_RUN_ONCE
                else do
                    -- use a 1ms timeout blocking poll on non-threaded rts
                    throwUVIfMinus_ (hs_uv_wake_up_timer loopData)
                    throwUVIfMinus_ (uv_run loop UV_RUN_ONCE)
            else throwUVIfMinus_ (uv_run loop UV_RUN_NOWAIT)

            (c, q) <- peekUVEventQueue loopData
            forM_ [0..c-1] $ \ i -> do
                slot <- peekElemOff q i
                lock <- indexArrM blockTable slot
                -- It's important to read the buffer size table inside running lock and
                -- unlock ghc thread with the result, where 'tryPutMVar' will mutate waiting
                -- thread's stack to ensure it will receive the result after get resumed.
                --
                -- After step finished, other threads are free to take the same slot,
                -- thus can overwrite the buffer size table, i.e. the previous result.
                --
                r <- peekBufferTable uvm slot
                tryPutMVar lock r
            return c

-- | Run a libuv FFI to get a 'UVSlotUnsafe' (which may exceed block table size),
-- resize the block table in that case, so that the returned slot always has an
-- accompanying 'MVar' in block table.
--
-- Always use this function to turn an 'UVSlotUnsafe' into 'UVSlot', so that the block
-- table size synchronize with libuv side's slot table.
getUVSlot :: HasCallStack => UVManager -> IO UVSlotUnsafe -> IO UVSlot
{-# INLINE getUVSlot #-}
getUVSlot (UVManager blockTableRef _ _ _ _) f = do
    slot <- throwUVIfMinus (unsafeGetSlot <$> f)
    blockTable <- readIORef blockTableRef
    let oldSiz = sizeofArr blockTable
    when (slot == oldSiz) $ do
        let newSiz = oldSiz `shiftL` 2
        blockTable' <- newArr newSiz
        copyArr blockTable' 0 blockTable 0 oldSiz
        forM_ [oldSiz..newSiz-1] $ \ i ->
            writeArr blockTable' i =<< newEmptyMVar
        !iBlockTable' <- unsafeFreezeArr blockTable'
        writeIORef blockTableRef iBlockTable'
    return slot

--------------------------------------------------------------------------------

-- | Cancel uv async function (actions which can be cancelled with 'uv_cancel') with
-- best effort, if the action is already performed, run an extra clean up action.
cancelUVReq :: UVManager -> UVSlot -> (Int -> IO ()) -> IO ()
cancelUVReq uvm slot extra_cleanup = withUVManager uvm $ \ loop -> do
    m <- getBlockMVar uvm slot
    r <- tryTakeMVar m
    case r of
        Just r' -> extra_cleanup r'             -- It's too late
        _ -> do
            pokeBufferTable uvm slot nullPtr 0  -- doing this let libuv side knows that
                                                -- we won't keep buffer alive in callbacks
            hs_uv_cancel loop slot              -- then we cancel the io with best efforts

-- | Exception safe uv request helper
--
-- This helper will run a libuv's async function, which will return a
-- libuv side's slot, then we will accommodate a 'MVar' in block table and
-- wait on that 'MVar', until the async function finished or an exception
-- is received, in later case we will call 'cancelUVReq' to cancel the on-going
-- async function with best efforts,
withUVRequest :: HasCallStack
              => UVManager -> (Ptr UVLoop -> IO UVSlotUnsafe) -> IO Int
withUVRequest uvm f = do
    (slot, m) <- withUVManager uvm $ \ loop -> mask_ $ do
        slot <- getUVSlot uvm (f loop)
        m <- getBlockMVar uvm slot
        _ <- tryTakeMVar m
        return (slot, m)
    throwUVIfMinus (takeMVar m `onException` cancelUVReq uvm slot no_extra_cleanup)
  where no_extra_cleanup = const $ return ()

-- | Same with 'withUVRequest' but disgard the result.
withUVRequest_ :: HasCallStack
               => UVManager -> (Ptr UVLoop -> IO UVSlotUnsafe) -> IO ()
withUVRequest_ uvm f = void (withUVRequest uvm f)

-- | Same with 'withUVRequest' but apply an convert function to result.
--
-- The convert function have all access to the returned value including
-- negative ones, it's convert funtions's responsiblity to throw an exception
-- if appropriate.
withUVRequest' :: HasCallStack
               => UVManager
               -> (Ptr UVLoop -> IO UVSlotUnsafe)
               -> (Int -> IO b)     -- ^ convert function
               -> IO b
withUVRequest' uvm f g = do
    (slot, m) <- withUVManager uvm $ \ loop -> mask_ $ do
        slot <- getUVSlot uvm (f loop)
        m <- getBlockMVar uvm slot
        -- since we locked uv manager here, it won't affect next event
        _ <- tryTakeMVar m
        return (slot, m)
    g =<< (takeMVar m `onException` cancelUVReq uvm slot no_extra_cleanup)
  where no_extra_cleanup = const $ return ()

-- | Same with 'withUVRequest', but will also run an extra cleanup function
-- if async exception hit this thread but the async action is already successfully performed,
-- e.g. release result memory.
withUVRequestEx :: HasCallStack
                => UVManager -> (Ptr UVLoop -> IO UVSlotUnsafe) -> (Int -> IO ()) -> IO Int
withUVRequestEx uvm f extra_cleanup = do
    (slot, m) <- withUVManager uvm $ \ loop -> mask_ $ do
        slot <- getUVSlot uvm (f loop)
        m <- getBlockMVar uvm slot
        _ <- tryTakeMVar m
        return (slot, m)
    throwUVIfMinus (takeMVar m `onException` cancelUVReq uvm slot extra_cleanup)

--------------------------------------------------------------------------------

-- | Fork a new GHC thread with active load-balancing.
--
-- Using libuv based IO solution has a disadvantage that file handlers are bound to certain
-- uv_loop, thus certain uv mananger/capability. Worker threads that migrate to other capability
-- will lead contention since various APIs here is protected by manager's lock, this makes GHC's
-- work-stealing strategy unsuitable for certain workload, such as a webserver.
-- we solve this problem with simple round-robin load-balancing: forkBa will automatically
-- distribute new threads to all capabilities in round-robin manner. Thus its name forkBa(lance).
forkBa :: IO () -> IO ThreadId
forkBa io = do
    i <- atomicAddCounter counter 1
    forkOn i io
  where
    counter :: Counter
    {-# NOINLINE counter #-}
    counter = unsafePerformIO $ newCounter 0

