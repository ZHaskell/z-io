{-|
Module      : Z.IO.Logger
Description : High performance logger
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Simple, high performance logger. The design choice of this logger is biased towards simplicity instead of generlization:

    * All log functions lives in 'IO'.
    * By default this logger is connected to stderr, use 'setStdLogger' to customize.
    * When logging each thread will build log 'Builder's into a small 'V.Bytes' with line buffer
      instead of leaving all 'Builder's to the flushing thread:
        * Logger won't keep heap data for too long simply because they're referenced by log's 'Builder'.
        * Each logging thread only need perform a CAS to prepend log 'V.Bytes' into a list,
          which reduces contention.
        * Each log call is atomic, Logging order is preserved under concurrent settings.

Flushing is automatic and throttled for 'debug', 'info', 'warn' to boost performance, but a 'fatal' log will always flush logger's buffer.  This could lead to a problem that if main thread exits too early logs may missed, to add a flushing when program exits, use 'withStdLogger' like:

@
import Z.IO.Logger

main :: IO ()
main = withStdLogger $ do
    ....
    debug "..."   -- So that this log won't be missed
    ...
@
-}

module Z.IO.Logger
  ( -- * A simple Logger type
    Logger(..)
  , LoggerConfig(..)
  , setStdLogger
  , getStdLogger
  , withStdLogger
  , newLogger
    -- * logging functions
  , debug
  , info
  , warn
  , fatal
  , otherLevel
    -- * logging functions with specific logger
  , debugTo
  , infoTo
  , warnTo
  , fatalTo
  , otherLevelTo
    -- * Helper to write new logger
  , defaultTSCache
  , defaultFmtCallStack
  , LogFormatter, defaultFmt
  , flushLog
  ) where

import Control.Monad
import Z.Data.Vector.Base as V
import Z.IO.LowResTimer
import Z.IO.StdStream
import Z.IO.Buffered
import System.IO.Unsafe (unsafePerformIO)
import Z.IO.Exception
import Data.IORef
import Control.Concurrent.MVar
import GHC.Stack
import qualified Z.Data.Builder as B
import qualified Data.Time as Time

type LogFormatter = Maybe (B.Builder ())     -- ^ data/time string
                    -> B.Builder ()          -- ^ log level
                    -> B.Builder ()          -- ^ log content
                    -> CallStack             -- ^ call stack trace
                    -> B.Builder ()

data Logger = Logger
    { loggerPushBuilder     :: B.Builder () -> IO () -- ^ push log into buffer
    , flushLogger           :: IO ()                -- ^ flush logger's buffer to output device
    , flushLoggerThrottled  :: IO ()                -- ^ throttled flush, e.g. use 'throttleTrailing_' from "Z.IO.LowResTimer"
    , loggerTSCache         :: IO (Maybe (B.Builder ())) -- ^ A IO action return a formatted date/time string
    , loggerFmt             :: LogFormatter
    }

data LoggerConfig = LoggerConfig
    { loggerMinFlushInterval :: {-# UNPACK #-} !Int -- ^ Minimal flush interval, see Notes on 'debug'
    , loggerLineBufSize      :: {-# UNPACK #-} !Int -- ^ Buffer size to build each log/line
    , loggerShowDebug        :: Bool                -- ^ Set to 'False' to filter debug logs
    , loggerShowTS           :: Bool                -- ^ Set to 'False' to disable auto data/time string prepending
    , loggerShowSourceLoc    :: Bool                -- ^ Set to 'True' to enable source location line
    }

-- | A default logger config with
--
--   * debug ON
--   * 0.1s minimal flush interval
--   * line buffer size 128 bytes
--   * show debug True
--   * show timestamp True
--   * don't show source location
--   * buffer size equals to 'V.defaultChunkSize'.
defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig 1 128 True True True

-- | A default timestamp cache with format @%Y-%m-%dT%H:%M:%S%Z@
--
-- The timestamp will updated in 0.1s granularity to ensure a seconds level precision.
defaultTSCache :: IO (B.Builder ())
{-# NOINLINE defaultTSCache #-}
defaultTSCache = unsafePerformIO $ do
    throttle 1 $ do
        t <- Time.getCurrentTime
        return . B.string8 $
            Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" t

-- | Use this function to implement a simple 'IORef' based concurrent logger.
--
-- @
-- bList <- newIORef []
-- let flush = flushLog buffered bList
-- ..
-- return $ Logger (pushLog bList) flush ...
-- @
--
flushLog :: (HasCallStack, Output o) => MVar (BufferedOutput o) -> IORef [V.Bytes] -> IO ()
flushLog oLock bList =
    withMVar oLock $ \ o -> do
        bss <- atomicModifyIORef' bList (\ bss -> ([], bss))
        forM_ (reverse bss) (writeBuffer o)
        flushBuffer o

-- | Make a new simple logger.
newLogger :: Output o
          => LoggerConfig
          -> MVar (BufferedOutput o)
          -> IO Logger
newLogger config oLock = do
    bList <- newIORef []
    let flush = flushLog oLock bList
    throttledFlush <- throttleTrailing_ (loggerMinFlushInterval config) flush
    return $ Logger (pushLog bList) flush throttledFlush tsCache (defaultFmt (loggerShowSourceLoc config))
  where
    tsCache = if (loggerShowTS config) then Just <$> defaultTSCache else pure Nothing
    pushLog bList b = do
        let !bs = B.buildBytesWith (loggerLineBufSize config) b
        atomicModifyIORef' bList (\ bss -> (bs:bss, ()))

-- | A default log formatter
--
-- @ [DEBUG][2020-10-09T07:44:14UTC][<interactive>:7:1]This a debug message@
defaultFmt :: Bool          -- ^ show call stack info?
           -> LogFormatter
defaultFmt showcstack maybeTS level content cstack = do
    B.square level
    forM_ maybeTS $ \ ts -> B.square ts
    when showcstack (B.square $ defaultFmtCallStack cstack)
    content

-- | Default stack formatter which fetch the logging source and location.
defaultFmtCallStack :: CallStack -> B.Builder ()
defaultFmtCallStack cs =
 case reverse $ getCallStack cs of
   [] -> "<no call stack found>"
   (_, loc):_ -> do
      B.string8 (srcLocFile loc)
      B.char8 ':'
      B.int (srcLocStartLine loc)
      B.char8 ':'
      B.int (srcLocStartCol loc)

globalLogger :: IORef Logger
{-# NOINLINE globalLogger #-}
globalLogger = unsafePerformIO $
    newIORef =<< newLogger defaultLoggerConfig stderrBuf

-- | Change the global logger.
setStdLogger :: Logger -> IO ()
setStdLogger !logger = atomicWriteIORef globalLogger logger

-- | Get the global logger.
getStdLogger :: IO Logger
getStdLogger = readIORef globalLogger

-- | Manually flush stderr logger.
flushDefaultLogger :: IO ()
flushDefaultLogger = getStdLogger >>= flushLogger

-- | Flush stderr logger when program exits.
withStdLogger :: IO () -> IO ()
withStdLogger = (`finally` flushDefaultLogger)

--------------------------------------------------------------------------------

debug :: HasCallStack => B.Builder () -> IO ()
debug = otherLevel_ "DEBUG" False callStack

info :: HasCallStack => B.Builder () -> IO ()
info = otherLevel_ "INFO" False callStack

warn :: HasCallStack => B.Builder () -> IO ()
warn = otherLevel_ "WARN" False callStack

fatal :: HasCallStack => B.Builder () -> IO ()
fatal = otherLevel_ "FATAL" True callStack

otherLevel :: HasCallStack
           => B.Builder ()      -- ^ log level
           -> Bool              -- ^ flush immediately?
           -> B.Builder ()      -- ^ log content
           -> IO ()
otherLevel level flushNow bu = otherLevel_ level flushNow callStack bu

otherLevel_ :: B.Builder () -> Bool -> CallStack -> B.Builder () -> IO ()
otherLevel_ level flushNow cstack bu = do
    logger <- getStdLogger
    otherLevelTo_ level flushNow cstack logger bu

--------------------------------------------------------------------------------

debugTo :: HasCallStack => Logger -> B.Builder () -> IO ()
debugTo = otherLevelTo_ "DEBUG" False callStack

infoTo :: HasCallStack => Logger -> B.Builder () -> IO ()
infoTo = otherLevelTo_ "INFO" False callStack

warnTo :: HasCallStack => Logger -> B.Builder () -> IO ()
warnTo = otherLevelTo_ "WARN" False callStack

fatalTo :: HasCallStack => Logger -> B.Builder () -> IO ()
fatalTo = otherLevelTo_ "FATAL" True callStack

otherLevelTo :: HasCallStack
             => Logger
             -> B.Builder ()      -- ^ log level
             -> Bool              -- ^ flush immediately?
             -> B.Builder ()      -- ^ log content
             -> IO ()
otherLevelTo logger level flushNow =
    otherLevelTo_ level flushNow callStack logger

otherLevelTo_ :: B.Builder () -> Bool -> CallStack -> Logger -> B.Builder () -> IO ()
otherLevelTo_ level flushNow cstack logger bu = do
    ts <- loggerTSCache logger
    (loggerPushBuilder logger) $ (loggerFmt logger) ts level bu cstack
    if flushNow
    then flushLogger logger
    else flushLoggerThrottled logger
