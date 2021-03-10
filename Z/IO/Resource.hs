{-|
Module      : Z.IO.Resource
Description : The Resource monad
Copyright   : (c) Dong Han, 2017
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module also implements Gabriel Gonzalez'd idea on 'Resource' applicative:
<http://www.haskellforall.com/2013/06/the-resource-applicative.html>. The 'Applicative' and 'Monad' instance is
especially useful when you want safely combine multiple resources.

A high performance resource pool based on STM is also provided.

-}

module Z.IO.Resource (
    -- * Resource management
    Resource(..)
  , initResource
  , initResource_
  , withResource
  , withResource'
    -- * Resource pool
  , Pool
  , initPool
  , withPool
  , SimplePool
  , initSimplePool
  , withSimplePool
  , statPool
  -- * Re-export
  , liftIO
) where

import           Control.Concurrent
import           Control.Monad
import qualified Control.Monad.Catch        as MonadCatch
import           Control.Monad.IO.Class
import qualified Data.Map.Strict            as M
import           Z.Data.PrimRef.PrimIORef
import           Z.Data.Array
import qualified Z.Data.Vector              as  V
import           Data.IORef
import           Z.IO.LowResTimer
import           Z.IO.Exception

--------------------------------------------------------------------------------

-- | A 'Resource' is an 'IO' action which acquires some resource of type a and
-- also returns a finalizer of type IO () that releases the resource.
--
-- The only safe way to use a 'Resource' is 'withResource' and 'withResource'',
-- You should not use the 'acquire' field directly, unless you want to implement your own
-- resource management. In the later case, you should 'mask_' 'acquire' since
-- some resource initializations may assume async exceptions are masked.
--
-- 'MonadIO' instance is provided so that you can lift 'IO' computation inside
-- 'Resource', this is convenient for propagating 'Resource' around since many
-- 'IO' computations carry finalizers.
--
-- A convention in Z-IO is that functions returning a 'Resource' should be
-- named in @initXXX@ format, users are strongly recommended to follow this convention.
--
-- There're two additional guarantees we made in Z-IO:
--
--   * All resources in Z-IO can track its own liveness, throw 'ResourceVanished'
--     exception using 'throwECLOSED' or 'throwECLOSEDSTM' when used after resource
--     is closed.
--
--   * All resources' clean up action in Z-IO is idempotent.
--
-- Library authors providing 'initXXX' are also encouraged to provide these guarantees.
--
newtype Resource a = Resource { acquire :: IO (a, IO ()) }

-- | Create 'Resource' from create and release action.
--
-- Note, 'resource' doesn't open resource itself, resource is created when you use
-- 'with' \/ 'with''.
--
initResource :: IO a -> (a -> IO ()) -> Resource a
{-# INLINE initResource #-}
initResource create release = Resource $ do
    r <- create
    return $ (r, release r)

-- | Create 'Resource' from create and release action.
--
-- This function is useful when you want to add some initialization and clean up action
-- inside 'Resource' monad.
--
initResource_ :: IO a -> IO () -> Resource a
{-# INLINE initResource_ #-}
initResource_ create release = Resource $ do
    r <- create
    return $ (r, release)

instance Functor Resource where
    {-# INLINE fmap #-}
    fmap f resource = Resource $ do
        (a, release) <- acquire resource
        return (f a, release)

instance Applicative Resource where
    {-# INLINE pure #-}
    pure a = Resource (pure (a, pure ()))
    {-# INLINE (<*>) #-}
    resource1 <*> resource2 = Resource $ do
        (f, release1) <- acquire resource1
        (x, release2) <- acquire resource2 `onException` release1
        return (f x, release2 >> release1)

instance Monad Resource where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    m >>= f = Resource $ do
        (m', release1) <- acquire m
        (x , release2) <- acquire (f m') `onException` release1
        return (x, release2 >> release1)

instance MonadIO Resource where
    {-# INLINE liftIO #-}
    liftIO f = Resource $ fmap (\ a -> (a, dummyRelease)) f
        where dummyRelease = return ()

-- | Create a new resource and run some computation, resource is guarantee to
-- be closed.
--
-- Be care don't leak the resource through computation return value, because
-- after the computation finishes, the resource is closed already.
--
withResource :: (MonadCatch.MonadMask m, MonadIO m, HasCallStack)
             => Resource a -> (a -> m b) -> m b
{-# INLINABLE withResource #-}
withResource resource k = MonadCatch.bracket
    (liftIO (acquire resource))
    (\(_, release) -> liftIO release)
    (\(a, _) -> k a)

-- | Create a new resource and run some computation, resource is guarantee to
-- be closed.
--
-- The difference from 'with' is that the computation will receive an extra
-- close action, which can be used to close the resource early before the whole
-- computation finished, the close action can be called multiple times,
-- only the first call will clean up the resource.
--
withResource' :: (MonadCatch.MonadMask m, MonadIO m, HasCallStack)
              => Resource a -> (a -> m () -> m b) -> m b
{-# INLINABLE withResource' #-}
withResource' resource k = do
    c <- liftIO (newCounter 0)
    MonadCatch.bracket
        (liftIO $ do
            (a, release) <- (acquire resource)
            let release' = do
                    c' <- atomicOrCounter c 1
                    when (c' == 0) release
            return (a, release'))
        (\(_, release) -> liftIO release)
        (\(a, release) -> k a (liftIO release))

--------------------------------------------------------------------------------

-- | A entry linked-list annotated with size.
data Entry res
    = EntryNil
    | EntryCons
        (res, IO ())            -- the resource and clean up action
        {-# UNPACK #-} !Int     -- size from this point on
        {-# UNPACK #-} !Int     -- the life remaining
        (Entry res)             -- next entry

-- | A high performance resource pool based on STM.
--
data Pool key res = Pool
    { _poolResource     :: key -> Resource res      -- ^ how to get a resource
    , _poolLimitPerKey  :: {-# UNPACK #-} !Int      -- ^ max number for resource we keep alive after used
    , _poolIdleTime     :: {-# UNPACK #-} !Int      -- ^ max idle time for resource we keep alive
    , _poolArray        :: {-# UNPACK #-} !(UnliftedArray (IORef (Maybe (M.Map key (Entry res)))))
    }

-- | Dump the status of pool.
statPool :: Pool key res -> IO (SmallArray (M.Map key Int))
statPool (Pool _ _ _ arr) = (`V.traverseVec` arr) $ \ resMapRef -> do
    mResMap <- readIORef resMapRef
    case mResMap of
        Just resMap -> return $ (`fmap` resMap) ( \ es ->
                case es of EntryCons _ siz _ _ -> siz
                           _                   -> 0)
        _ -> throwECLOSED

-- | Initialize a resource pool with given 'Resource'
--
-- Like other initXXX functions, this function won't open a resource pool until you use 'withResource'.
initPool :: (key -> Resource res)
         -> Int     -- ^ maximum number of resources per local pool per key can be opened
         -> Int     -- ^ amount of time after which an unused resource can be released (in seconds).
         -> Resource (Pool key res)
initPool resf limit itime = initResource createPool closePool
  where
    createPool = do
        numCaps <- getNumCapabilities
        marr <- newArr numCaps
        forM_ [0..numCaps-1] $ \ i -> do
            writeArr marr i =<< newIORef (Just M.empty)
        arr <- unsafeFreezeArr marr
        return (Pool resf limit itime arr)

    closePool (Pool _ _ _ localPoolArr) = do
        -- close all existed resource
        (`V.traverseVec_` localPoolArr) $ \ resMapRef ->
            atomicModifyIORef resMapRef $ \ mResMap ->
                case mResMap of
                    Just resMap -> (Nothing, mapM_ closeEntry resMap)
                    _ -> (Nothing, return ())

    closeEntry (EntryCons (_, close) _ _ _) = ignoreSync close
    closeEntry EntryNil = return ()

-- | Open resource inside a given resource pool and do some computation.
--
-- This function is thread safe, concurrently usage will be guaranteed
-- to get different resource. If exception happens,
-- resource will be closed(not return to pool).
withPool :: (MonadCatch.MonadMask m, MonadIO m, Ord key, HasCallStack)
                   => Pool key res -> key -> (res -> m a) -> m a
withPool (Pool resf limitPerKey itime arr) key f = do
    !resMapRef <- indexArr arr . fst <$> liftIO (threadCapability =<< myThreadId)
    fst <$> MonadCatch.generalBracket
        (liftIO $ takeFromPool resMapRef)
        (\ r@(_, close) exit ->
            case exit of
                MonadCatch.ExitCaseSuccess _ -> liftIO (returnToPool resMapRef r)
                _ -> liftIO close)
        (\ (a, _) -> f a)
  where
    takeFromPool resMapRef =
        join . atomicModifyIORef' resMapRef $ \ mResMap ->
            case mResMap of
                Just resMap ->
                    case M.lookup key resMap of
                        Just (EntryCons a _ _ es') ->
                            (Just $! M.adjust (const es') key resMap, return a)
                        Just EntryNil ->
                            (Just $! M.delete key resMap, acquire (resf key))
                        _ ->  (Just resMap, acquire (resf key))

                _ -> (Nothing, throwECLOSED)

    returnToPool resMapRef r = do
        join . atomicModifyIORef' resMapRef $ \ mResMap ->
            case mResMap of
                Just resMap ->
                    case M.lookup key resMap of
                        Just (EntryCons _ siz _ _) ->
                            if siz < limitPerKey
                            -- if entries under given key do not exceed limit, we prepend res back to entries
                            then (Just $! M.adjust (EntryCons r (siz+1) itime) key resMap, return ())
                            -- otherwise we close it
                            else (Just resMap, snd r)
                        _ -> (Just $! M.insert key (EntryCons r 1 itime EntryNil) resMap,
                                scanLocalPool resMapRef)
                _ -> (Nothing, snd r)

    scanLocalPool resMapRef = do
        registerLowResTimer_ 10 . join . atomicModifyIORef' resMapRef $ \ mResMap ->
            case mResMap of
                Just resMap ->
                    case M.lookup key resMap of
                        Just es -> do
                            let (dead, living) = age es 0 [] EntryNil
                            case living of
                                -- no living resources any more, stop scanning
                                EntryNil -> (Just $! M.delete key resMap,
                                    forM_ dead (ignoreSync . snd))
                                _ ->  (Just $! M.adjust (const living) key resMap,
                                    (do forM_ dead (ignoreSync . snd)
                                        scanLocalPool resMapRef))
                        -- no living resources under given key, stop scanning
                        _ -> (Just resMap, return ())
                _ -> (Nothing, return ())

    age (EntryCons a _ life es) !livingNum dead living
        | life > 1  = let !livingNum' = (livingNum+1)
                      in age es livingNum' dead (EntryCons a livingNum' (life-1) living)
        | otherwise = age es livingNum (a:dead) living
    age _ _ dead living = (dead, living)

-- | Simple resource pool where lookup via key is not needed.
type SimplePool res = Pool () res

-- | Initialize a 'SimplePool'.
initSimplePool :: Resource res
               -> Int     -- ^ maximum number of resources per local pool can be opened
               -> Int     -- ^ amount of time after which an unused resource can be released (in seconds).
               -> Resource (SimplePool res)
initSimplePool f = initPool (const f)

-- | Open resource with 'SimplePool', see 'withPool'
--
withSimplePool :: (MonadCatch.MonadMask m, MonadIO m, HasCallStack)
                   => SimplePool res -> (res -> m a) -> m a
withSimplePool pool = withPool pool ()
