{-|
Module      : Z.IO.StdStream
Description : Standard Streams and TTY devices
Copyright   : (c) Dong Han, 2018-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides stdin\/stderr\/stdout reading and writings. Usually you don't have to use 'stderr' or 'stderrBuf' directly, 'Z.IO.Logger' provides more logging utilities through @stderr@. While 'stdinBuf' and 'stdoutBuf' is useful when you write interactive programs, 'Z.IO.Buffered' module provide many reading and writing operations. Example:

@
import Control.Concurrent.MVar
import Z.IO.LowResTimer
import Z.IO.Buffered
import Z.IO.StdStream
import qualified Z.Data.Vector as V
import qualified Z.Data.Builder as B
main = do
    -- read by '\n'
    b1 <- readStd
    -- read whatever user input in 3s, otherwise get Nothing
    b2 <- timeoutLowRes 30 $ withMVar stdinBuf readBuffer
    ...
    putStd "hello world!"

    -- Raw mode
    setStdinTTYMode UV_TTY_MODE_RAW
    forever $ do
        withMVar stdinBuf $ \ i -> withMVar stdoutBuf $ \ o -> do
            bs <- readBuffer i
            let Just key = V.headMaybe bs
            writeBuilder o (B.hex key)
            flushBuffer o
@

-}
module Z.IO.StdStream
  ( -- * Standard input & output streams
    StdStream
  , isStdStreamTTY
  , getStdStreamFD
  , setStdinTTYMode
  , getStdoutWinSize
  , stdin, stdout, stderr
  , stdinBuf, stdoutBuf, stderrBuf
    -- * utils
  , readStd, printStd, putStd
    -- * re-export
  , withMVar
  -- * Constant
  -- ** TTYMode
  , TTYMode
  , pattern TTY_MODE_NORMAL
  , pattern TTY_MODE_RAW
  ) where

import Control.Monad
import Control.Concurrent.MVar
import Foreign.Ptr
import System.IO.Unsafe
import qualified Z.Data.Builder             as B
import qualified Z.Data.Text.Print          as T
import qualified Z.Data.Vector              as V
import Z.IO.UV.FFI
import Z.IO.UV.Manager
import Z.IO.UV.Errno
import Z.IO.Exception
import Z.IO.Buffered
import Z.Foreign

-- | Standard input and output streams
--
-- We support both regular file and TTY based streams, when initialized
-- 'uv_guess_handle' is called to decide which type of devices are connected
-- to standard streams.
--
-- Note 'StdStream' is not thread safe, you shouldn't use them without lock.
-- For the same reason you shouldn't use stderr directly, use `Z.IO.Logger` module instead.

data StdStream
    = StdTTY {-# UNPACK #-}!(Ptr UVHandle) {-# UNPACK #-}!UVSlot UVManager -- similar to UVStream
    | StdFile {-# UNPACK #-}!FD                                          -- similar to UVFile

instance Show StdStream where show = T.toString

instance T.Print StdStream where
    toUTF8BuilderP p (StdTTY ptr slot uvm) = T.parenWhen (p > 10) $ do
        "StdTTY "
        T.toUTF8Builder ptr
        T.char7 ' '
        T.toUTF8Builder slot
        T.char7 ' '
        T.toUTF8BuilderP 11 uvm
    toUTF8BuilderP p (StdFile fd) = T.parenWhen (p > 10) $ do
        "StdFile "
        T.toUTF8Builder fd

isStdStreamTTY :: StdStream -> Bool
isStdStreamTTY (StdTTY _ _ _) = True
isStdStreamTTY _              = False

getStdStreamFD :: StdStream -> IO FD
getStdStreamFD (StdTTY hdl _ _) = throwUVIfMinus (hs_uv_fileno hdl)
getStdStreamFD (StdFile fd) = return fd

instance Input StdStream where
    {-# INLINE readInput #-}
    readInput (StdTTY hdl slot uvm) buf len = mask_ $ do
        pokeBufferTable uvm slot buf len
        m <- getBlockMVar uvm slot
        _ <- tryTakeMVar m
        throwUVIfMinus_ $ withUVManager' uvm (hs_uv_read_start hdl)
        -- since we are inside mask, this is the only place
        -- async exceptions could possibly kick in, and we should stop reading
        r <- takeMVar m `onException` (do
                -- normally we call 'uv_read_stop' in C read callback
                -- but when exception raise, here's the place to stop
                -- stop a handle twice will be a libuv error, so we don't check result
                _ <- withUVManager' uvm (uv_read_stop hdl)
                void (tryTakeMVar m))
        if  | r > 0  -> return r
            | r == fromIntegral UV_EOF -> return 0
            | r < 0 ->  throwUVIfMinus (return r)
            -- r == 0 should be impossible, since we guard this situation in c side
            | otherwise -> throwUVError UV_UNKNOWN IOEInfo{
                                  ioeName = "StdStream read error"
                                , ioeDescription = "StdStream read should never return 0 before EOF"
                                , ioeCallStack = callStack
                                }
    readInput (StdFile fd) buf len =
        throwUVIfMinus $ hs_uv_fs_read fd buf len (-1)

instance Output StdStream where
    {-# INLINE writeOutput #-}
    writeOutput (StdTTY hdl _ uvm) buf len = mask_ $ do
        m <- withUVManager' uvm $ do
            reqSlot <- getUVSlot uvm (hs_uv_write hdl buf len)
            m <- getBlockMVar uvm reqSlot
            _ <- tryTakeMVar m
            return m
        -- we can't cancel uv_write_t with current libuv,
        -- otherwise disaster will happen if buffer got collected.
        -- so we have to turn to uninterruptibleMask_'s help.
        -- i.e. writing UVStream is an uninterruptible operation.
        -- OS will guarantee writing TTY and socket will not
        -- hang forever anyway.
        throwUVIfMinus_ (uninterruptibleMask_ $ takeMVar m)
    writeOutput (StdFile fd) buf len = go buf len
      where
        go !b !bufSiz = do
            written <- throwUVIfMinus
                (hs_uv_fs_write fd b bufSiz (-1))
            when (written < bufSiz)
                (go (b `plusPtr` written) (bufSiz-written))

-- | The global stdin stream.
stdin :: StdStream
{-# NOINLINE stdin #-}
stdin = unsafePerformIO (makeStdStream 0)

-- | The global stdout stream.
--
-- | If you want to write logs, don't use 'stdout' directly, use 'Z.IO.Logger' instead.
stdout :: StdStream
{-# NOINLINE stdout #-}
stdout = unsafePerformIO (makeStdStream 1)

-- | The global stderr stream.
--
-- | If you want to write logs, don't use 'stderr' directly, use 'Z.IO.Logger' instead.
stderr :: StdStream
{-# NOINLINE stderr #-}
stderr = unsafePerformIO (makeStdStream 2)

-- |  A global buffered stdin stream protected by 'MVar'.
stdinBuf :: MVar BufferedInput
{-# NOINLINE stdinBuf #-}
stdinBuf = unsafePerformIO (newBufferedInput stdin >>= newMVar)

-- |  A global buffered stdout stream protected by 'MVar'.
--
-- | If you want to write logs, don't use 'stdoutBuf' directly, use 'Z.IO.Logger' instead.
stdoutBuf :: MVar BufferedOutput
{-# NOINLINE stdoutBuf #-}
stdoutBuf = unsafePerformIO (newBufferedOutput stdout >>= newMVar)

-- |  A global buffered stderr stream protected by 'MVar'.
--
-- | If you want to write logs, don't use 'stderrBuf' directly, use 'Z.IO.Logger' instead.
stderrBuf :: MVar BufferedOutput
{-# NOINLINE stderrBuf #-}
stderrBuf = unsafePerformIO (newBufferedOutput stderr >>= newMVar)

makeStdStream :: HasCallStack => FD -> IO StdStream
makeStdStream fd = do
    typ <- uv_guess_handle fd
    if typ == UV_TTY
    then mask_ $ do
        uvm <- getUVManager
        withUVManager uvm $ \ loop -> do
            hdl <- hs_uv_handle_alloc loop
            slot <- getUVSlot uvm (peekUVHandleData hdl)
            _ <- tryTakeMVar =<< getBlockMVar uvm slot   -- clear the parking spot
            throwUVIfMinus_ (uv_tty_init loop hdl (fromIntegral fd))
            return (StdTTY hdl slot uvm)
    else return (StdFile fd)

-- | Change terminal's mode if stdin is connected to a terminal.
setStdinTTYMode :: TTYMode -> IO ()
setStdinTTYMode mode = case stdin of
    StdTTY hdl _ uvm ->
        withUVManager' uvm . throwUVIfMinus_ $ uv_tty_set_mode hdl mode
    _ -> return ()

-- | Get terminal's output window size in (width, height) format,
-- return (-1, -1) if stdout is a file.
getStdoutWinSize :: HasCallStack => IO (CInt, CInt)
getStdoutWinSize = case stdout of
    StdTTY hdl _ uvm ->
        withUVManager' uvm $ do
            (w, (h, ())) <- allocPrimUnsafe $ \ w ->
                allocPrimUnsafe $ \ h -> throwUVIfMinus_ $ uv_tty_get_winsize hdl w h
            return (w, h)
    _ -> return (-1, -1)

--------------------------------------------------------------------------------

-- | Print a 'Print' and flush to stdout, with a linefeed.
printStd :: (HasCallStack, T.Print a) => a -> IO ()
printStd s = putStd (T.toUTF8Builder s)

-- | Print a 'Builder' and flush to stdout, with a linefeed.
putStd :: HasCallStack => B.Builder a -> IO ()
putStd b = withMVar stdoutBuf $ \ o -> do
    writeBuilder o (b >> B.char8 '\n')
    flushBuffer o

-- | Read a line from stdin
--
-- This function will throw 'ECLOSED' when meet EOF, which may cause trouble if stdin is connected
-- to a file, use 'readLine' instead.
readStd :: HasCallStack => IO V.Bytes
readStd = withMVar stdinBuf $ \ s -> do
    line <- readLine s
    case line of Just line' -> return line'
                 Nothing    -> throwIO (ResourceVanished
                    (IOEInfo "ECLOSED" "stdin is closed" callStack))

