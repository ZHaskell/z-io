{-|
Module      : Z.IO.FileSystem.Watch
Description : cross-platform recursive fs watcher
Copyright   : (c) Dong Han, 2017-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides fs watcher based on libuv's fs_event, we also maintain watch list if target OS doesn't
support recursive watch(Linux's inotify).

@
-- start watching threads, cleanup watching threads automatically when finished.
withResource (initWatchDirs ["fold_to_be_watch"] True) $ \ srcf -> do
    -- dup a file event source
    src <- srcf
    -- print event to stdout
    runBIO $ src >|> sinkToIO printStd
@
-}

module Z.IO.FileSystem.Watch
    ( FileEvent(..)
    , watchDirs
    , initWatchDirs
    ) where

import           Control.Concurrent
import           Control.Monad
import           Data.Bits
import qualified Data.HashMap.Strict      as HM
import           Data.IORef
import qualified Data.List                as List
import           Data.Primitive.PrimArray
import           Data.Word
import           GHC.Generics
import           Z.Data.Array
import           Z.Data.Array.Unaligned
import           Z.Data.CBytes            (CBytes)
import qualified Z.Data.CBytes            as CBytes
import           Z.Data.JSON              (JSON)
import           Z.Data.Text.Print        (Print)
import           Z.Data.Vector            (defaultChunkSize)
import           Z.Foreign
import           Z.IO.BIO
import           Z.IO.BIO.Concurrent
import           Z.IO.Exception
import           Z.IO.FileSystem.Base
import qualified Z.IO.FileSystem.FilePath as P
import           Z.IO.LowResTimer
import           Z.IO.UV.FFI
import           Z.IO.UV.Manager
import           Z.IO.Resource

-- | File event with path info.
data FileEvent = FileAdd CBytes | FileRemove CBytes | FileModify CBytes
    deriving (Show, Read, Ord, Eq, Generic)
    deriving anyclass (Print, JSON)

-- | Watching a list of given directories.
watchDirs :: [CBytes]     -- ^ Directories to be watched
                -> Bool         -- ^ recursively watch?
                -> (FileEvent -> IO ())  -- ^ Callback function to handle 'FileEvent'
                -> IO ()
watchDirs dirs rec callback = do
    withResource (initWatchDirs dirs rec) $ \ srcf -> do
        src <- srcf
        runBIO $ src >|> sinkToIO callback

-- | Start watching a list of given directories, stream version.
initWatchDirs :: [CBytes]       -- ^ watching list
          -> Bool           -- ^ recursively watch?
          -> Resource (IO (Source FileEvent))
initWatchDirs dirs False = do
    liftIO . forM_ dirs $ \ dir -> do
        b <- isDir dir
        unless b (throwUVIfMinus_ (return UV_ENOTDIR))
    watch_ 0 dirs
initWatchDirs dirs _ = do
#if defined(linux_HOST_OS)
    -- inotify doesn't support recursive watch, so we manually maintain watch list
    subDirs <- liftIO . forM dirs $ \ dir ->
        scandirRecursively dir (\ _ t -> return (t == DirEntDir))
    watch_ UV_FS_EVENT_RECURSIVE (List.concat (dirs:subDirs))
#else
    watch_ UV_FS_EVENT_RECURSIVE dirs
#endif

-- Internal function to start watching
watch_ :: CUInt -> [CBytes] -> Resource (IO (Source FileEvent))
watch_ flag dirs = fst <$> initResource (do
    -- HashMap to store all watchers
    mRef <- newMVar HM.empty
    -- there's only one place to pull the sink, that is cleanUpWatcher
    (sink, srcf) <- newBroadcastTChanNode 1
    -- lock UVManager first
    (forM_ dirs $ \ dir -> do
        dir' <- P.normalize dir
        tid <- forkIO $ watchThread mRef dir' sink
        modifyMVar_ mRef $ \ m ->
            return $! HM.insert dir' tid m) `onException` cleanUpWatcher mRef sink
    return (srcf, (sink, mRef)))
    (\ (_, (sink, mRef)) -> cleanUpWatcher mRef sink)
  where
    eventBufSiz = defaultChunkSize

    cleanUpWatcher mRef sink = do
        m <- takeMVar mRef
        forM_ m killThread
        void (pull sink)

    watchThread mRef dir sink = do
        -- IORef store temp events to de-duplicated
        eRef <- newIORef Nothing
        uvm <- getUVManager
        (bracket
            (do withUVManager uvm $ \ loop -> do
                    hdl <- hs_uv_handle_alloc loop
                    slot <- getUVSlot uvm (peekUVHandleData hdl)
                    -- init uv struct
                    throwUVIfMinus_ (uv_fs_event_init loop hdl)

                    buf <- newPinnedPrimArray eventBufSiz :: IO (MutablePrimArray RealWorld Word8)

                    check <- throwOOMIfNull $ hs_uv_check_alloc
                    throwUVIfMinus_ (hs_uv_check_init check hdl)

                    withMutablePrimArrayContents buf $ \ p -> do
                        pokeBufferTable uvm slot (castPtr p) eventBufSiz
                        -- init uv_check_t must come after poking buffer
                        throwUVIfMinus_ $ hs_uv_fs_event_check_start check

                    return (hdl, slot, buf, check))

            (\ (hdl,_,_,check) -> hs_uv_handle_close hdl >> hs_uv_check_close check)

            (\ (hdl, slot, buf, _) -> do
                m <- getBlockMVar uvm slot
                withUVManager' uvm $ do
                    _ <- tryTakeMVar m
                    pokeBufferSizeTable uvm slot eventBufSiz
                    CBytes.withCBytesUnsafe dir $ \ p ->
                        throwUVIfMinus_ (hs_uv_fs_event_start hdl p flag)

                forever $ do

                    _ <- takeMVar m `onException` (do
                            _ <- withUVManager' uvm $ uv_fs_event_stop hdl
                            void (tryTakeMVar m))

                    (PrimArray buf#) <- withUVManager' uvm $ do
                        _ <- tryTakeMVar m
                        r <- peekBufferSizeTable uvm slot
                        pokeBufferSizeTable uvm slot eventBufSiz

                        let eventSiz = eventBufSiz - r
                        buf' <- newPrimArray eventSiz
                        copyMutablePrimArray buf' 0 buf r eventSiz
                        unsafeFreezePrimArray buf'

                    forkIO $ processEvent dir mRef eRef sink =<< loopReadFileEvent buf# 0 [])
            ) `catch`
                -- when a directory is removed, either watcher is killed
                -- or hs_uv_fs_event_start return ENOENT
                (\ (_ :: NoSuchThing) -> return ())

    loopReadFileEvent buf# i acc
        | i >= siz = return acc
        | otherwise =
            let !event  = indexBA buf# i
                !path   = CBytes.indexBACBytes buf# (i + 1)
            in loopReadFileEvent buf# (i + CBytes.length path + 2) ((event,path):acc)
      where siz = sizeofPrimArray (PrimArray buf# :: PrimArray Word8)

    processEvent pdir mRef eRef sink = mapM_ $ \ (e, path) ->
        -- don't report event about directory itself, it will reported by its parent
        unless (CBytes.null path) $ do
            f <- pdir `P.join` path
            if (e .&. UV_RENAME) /= 0
            then catch
                (do _s <- lstat f
#if defined(linux_HOST_OS)
                    when ((stMode _s .&. S_IFMT == S_IFDIR) && (flag .&. UV_FS_EVENT_RECURSIVE /= 0)) $ do
                        modifyMVar_ mRef $ \ m -> do
                            case HM.lookup f m of
                                Just _ -> return m
                                _ -> do
                                    ds <- scandirRecursively f (\ _ t -> return (t == DirEntDir))
                                    foldM (\ m' d -> do
                                        tid <- forkIO $ watchThread mRef d sink
                                        return $! HM.insert d tid m') m (f:ds)
#endif
                    pushDedup eRef sink (FileAdd f))
                (\ (_ :: NoSuchThing) -> do
                    modifyMVar_ mRef $ \ m -> do
                        forM_ (HM.lookup f m) killThread
                        return (HM.delete f m)
                    pushDedup eRef sink (FileRemove f))
            else pushDedup eRef sink (FileModify f)

    pushDedup eRef sink event = do
        registerLowResTimer_ 1 $ do
            me' <- atomicModifyIORef' eRef $ \ me ->
                case me of
                    Just e -> (Nothing, Just e)
                    _      -> (Nothing, Nothing)
            forM_ me' (push sink)

        me' <- atomicModifyIORef' eRef $ \ me ->
            case me of
                Just e -> if (e == event)
                    then (me, Nothing)
                    else (Just event, Just e)
                _ -> (Just event, Nothing)
        forM_ me' (push sink)
