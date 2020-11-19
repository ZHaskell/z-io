module Z.IO.FileSystem.Watch where

import Control.Concurrent
import Control.Monad
import Z.Data.Array
import Data.Word
import Data.Bits
import Z.Data.ShowT (ShowT)
import Z.Data.JSON (FromValue, ToValue, EncodeJSON)
import Foreign.Storable (peek)
import Foreign.Ptr (plusPtr)
import GHC.Generics
import Z.Foreign
import Z.IO.Exception
import Z.IO.FileSystem
import qualified Z.IO.FileSystem.FilePath as P
import Z.IO.BIO
import Z.IO.BIO.Concurrent
import Z.IO.UV.FFI
import Z.IO.UV.Manager
import qualified Z.Data.CBytes          as CBytes
import           Z.Data.CBytes          (CBytes)
import Z.Data.Vector                    (defaultChunkSize)
import qualified Data.HashMap.Strict    as HM

data FileEvent
    = FileAdd    CBytes
    | FileRemove CBytes
    | FileModify CBytes
  deriving (Show, Read, Ord, Eq, Generic)
  deriving anyclass (ShowT, FromValue, ToValue, EncodeJSON)

-- | Start watch a given file or directory recursively.
--
watchDir :: CBytes -> IO (IO (), IO (Source FileEvent))
watchDir dir = do
    b <- isDir dir
    unless b (throwUVIfMinus_ (return UV_ENOTDIR))
#if defined(linux_HOST_OS)
    -- inotify doesn't support recursive watch, so we manually maintain watch list
    watchDirs_ 0 =<< getAllDirs_ dir []
#else
    watchDirs_ UV_FS_EVENT_RECURSIVE [dir]
#endif
  where
    getAllDirs_ pdir acc = do
        foldM (\ acc' (d,t) -> if (t == DirEntDir)
                then do
                    d' <- pdir `P.join` d
                    (getAllDirs_ d' (d':acc'))
                else return acc'
            ) acc =<< scandir pdir


watchDirs_ :: CUInt -> [CBytes] -> IO (IO (), IO (Source FileEvent))
watchDirs_ flag dirs = do
    mRef <- newMVar HM.empty
    (sink, srcf) <- newBroadcastTChanNode 1 -- there's only one place to pull the sink
    -- lock UVManager first
    forM_ dirs $ \ dir -> do
        dir' <- P.normalize dir
        tid <- forkIO $ watchThread mRef dir' sink
        modifyMVar_ mRef $ \ m -> return $! HM.insert dir' tid m
    return (cleanUpWatcher mRef sink, srcf)
  where
    eventBufSiz = defaultChunkSize

    cleanUpWatcher mRef sink = do
        m <- takeMVar mRef
        forM_ m killThread
        void (pull sink)

    watchThread mRef dir sink = do
        uvm <- getUVManager
        bracket
            (do withUVManager uvm $ \ loop -> do
                    hdl <- hs_uv_handle_alloc loop
                    slot <- getUVSlot uvm (peekUVHandleData hdl)
                    -- init uv struct
                    throwUVIfMinus_ (uv_fs_event_init loop hdl)

                    buf <- newPrimArray eventBufSiz :: IO (MutablePrimArray RealWorld Word8)

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
                forever $ do

                    throwUVIfMinus_ . withUVManager' uvm $ do
                        _ <- tryTakeMVar m
                        pokeBufferSizeTable uvm slot eventBufSiz

                        CBytes.withCBytesUnsafe dir $ \ p ->
                            hs_uv_fs_event_start hdl p flag

                    r <- takeMVar m `onException` (do
                            _ <- withUVManager' uvm $ uv_fs_event_stop hdl
                            void (tryTakeMVar m))

                    events <- withMutablePrimArrayContents buf $ \ p -> do
                        loopReadFileEvent (p `plusPtr` r) (p `plusPtr` eventBufSiz) []

                    processEvent dir mRef sink events)

    loopReadFileEvent p pend acc
        | p >= pend = return acc
        | otherwise = do
            event   <- peek p
            path    <- CBytes.fromCString (p `plusPtr` 1)
            loopReadFileEvent (p `plusPtr` (CBytes.length path + 2)) pend ((event,path):acc)

    processEvent pdir mRef sink = mapM_ $ \ (e, path) -> do
        dir <- pdir `P.join` path
        if (e .&. UV_RENAME) /= 0
        then do
            push sink =<< catch
                (do s <- lstat dir
                    when (stMode s .&. S_IFMT == S_IFDIR) (do
                        modifyMVar_ mRef $ \ m -> do
                            case HM.lookup dir m of
                                Just _ -> return m
                                _ -> do
                                    tid <- forkIO $ watchThread mRef dir sink
                                    return $! HM.insert dir tid m)
                    return (FileAdd dir))
                (\ (_ :: NoSuchThing) -> do
                    modifyMVar_ mRef $ \ m -> do
                        forM_ (HM.lookup dir m) killThread
                        return (HM.delete dir m)
                    return (FileRemove dir))
        else push sink (FileModify dir)
