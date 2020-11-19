module Z.IO.FileSystem.Watch where

import Control.Monad
import Control.Concurrent.MVar
import Z.Data.Array
import Data.Word
import Foreign.Storable (peek)
import Foreign.Ptr (plusPtr)
import Z.Foreign
import Z.IO.Exception
import Z.IO.FileSystem
import Z.IO.BIO
import Z.IO.BIO.Concurrent
import Z.IO.UV.FFI
import Z.IO.UV.Manager
import qualified Z.Data.CBytes          as CBytes
import           Z.Data.CBytes          (CBytes)
import Z.Data.Vector                    (defaultChunkSize)

data FileEvent
    = FileAdd CBytes FStat
    | FileRemove CBytes FStat
    | FileMove CBytes FStat CBytes FStat

-- | Start watch a given file or directory.
--
watchFiles :: CBytes -> (FileEvent -> IO a) -> IO ()
watchFiles dir callback = do
    uvm <- getUVManager
    (hdl, slot) <- withUVManager uvm $ \ loop -> do
        bracketOnError
            (hs_uv_handle_alloc loop)
            hs_uv_handle_free
            ( \ hdl -> do
                slot <- getUVSlot uvm (peekUVHandleData hdl)
                -- clean up
                _ <- tryTakeMVar =<< getBlockMVar uvm slot
                -- init uv struct
                throwUVIfMinus_ (uv_fs_event_init loop hdl)
                return (hdl, slot))

    buf <- newPrimArray eventBufSiz :: IO (MutablePrimArray RealWorld Word8)

    bracket
        (do check <- throwOOMIfNull $ hs_uv_check_alloc
            throwUVIfMinus_ (hs_uv_check_init check hdl)
            return check)
        hs_uv_check_close $
        \ check -> do

            withMutablePrimArrayContents buf $ \ p -> do
                pokeBufferTable uvm slot (castPtr p) eventBufSiz
                -- init uv_check_t must come after poking buffer
                throwUVIfMinus_ $ hs_uv_fs_event_check_start check

            m <- getBlockMVar uvm slot

            forever $ do

                throwUVIfMinus_ . withUVManager' uvm $ do
                    _ <- tryTakeMVar m
                    pokeBufferSizeTable uvm slot eventBufSiz
                    CBytes.withCBytesUnsafe dir $ \ p ->
                        hs_uv_fs_event_start hdl p UV_FS_EVENT_RECURSIVE

                r <- takeMVar m `onException` (do
                        -- TODO
                        withUVManager' uvm $ do
                            throwUVIfMinus_ (uv_fs_event_stop hdl)
                            hs_uv_handle_free hdl
                        void (tryTakeMVar m))

                events <- withMutablePrimArrayContents buf $ \ p -> do
                    loopReadFileEvent (p `plusPtr` r) (p `plusPtr` eventBufSiz) []


                print (events :: [(Word8, CBytes)])


  where
    eventBufSiz = defaultChunkSize
    loopReadFileEvent p pend acc
        | p >= pend = return acc
        | otherwise = do
            event   <- peek p
            path    <- CBytes.fromCString (p `plusPtr` 1)
            loopReadFileEvent (p `plusPtr` (CBytes.length path + 2)) pend ((event,path):acc)
