{-|
Module      : Z.IO.Logger
Description : High performance logger
Copyright   : (c) Dong Han, 2017-2018
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Simple, high performance logger. The design choice of this logger is biased towards simplicity instead of generlization:

* All log functions live in 'IO'.
* By default this logger is connected to stderr, use 'setDefaultLogger' to customize.
* When logging each thread will build log 'Builder's into a small 'V.Bytes' with line buffer instead of leaving all 'Builder's to the flushing thread:

    * Logger won't keep heap data for too long simply because they're referenced by log's 'Builder'.
    * Each logging thread only need perform a CAS to prepend log 'V.Bytes' into a list, which reduces contention.
    * Each log call is atomic, Logging order is preserved under concurrent settings.

Flushing is automatic and throttled for 'debug', 'info', 'warn' to boost performance, but a 'fatal' log will always flush logger's buffer.  This could lead to a problem that if main thread exits too early logs may missed, to add a flushing when program exits, use 'withDefaultLogger' like:

@
import Z.IO.Logger

main :: IO ()
main = withDefaultLogger $ do
    ....
    debug "..."   -- So that this log won't be missed
    ...
@
-}

module Z.IO.Logger
  ( -- * A simple Logger type
    Logger(..)
  , LoggerConfig(..)
  , defaultLoggerConfig
  , setDefaultLogger
  , getDefaultLogger
  , flushDefaultLogger
  , withDefaultLogger
  , newLogger
  , newColoredLogger
    -- * logging functions
  , debug
  , info
  , warn
  , fatal
  , otherLevel
  , Level
    -- * logging functions with specific logger
  , debugTo
  , infoTo
  , warnTo
  , fatalTo
  , otherLevelTo
    -- * Helper to write new logger
  , defaultTSCache
  , defaultFmtCallStack
  , LogFormatter, defaultFmt, coloredFmt
  , flushLog
  ) where

import Control.Monad
import Z.Data.Vector.Base as V
import Z.IO.LowResTimer
import Z.IO.StdStream
import Z.IO.StdStream.Ansi  (color, AnsiColor(..))
import Z.IO.Buffered
import System.IO.Unsafe (unsafePerformIO)
import Z.IO.Exception
import Z.IO.Time
import Data.IORef
import Control.Concurrent.MVar
import GHC.Stack
import qualified Z.Data.Builder as B
import qualified Z.Data.CBytes as CB

type LogFormatter = B.Builder ()            -- ^ data\/time string
                  -> Level                  -- ^ log level
                  -> B.Builder ()           -- ^ log content
                  -> CallStack              -- ^ call stack trace
                  -> B.Builder ()

data Logger = Logger
    { loggerPushBuilder     :: B.Builder () -> IO () -- ^ push log into buffer
    , flushLogger           :: IO ()                -- ^ flush logger's buffer to output device
    , flushLoggerThrottled  :: IO ()                -- ^ throttled flush, e.g. use 'throttleTrailing_' from "Z.IO.LowResTimer"
    , loggerTSCache         :: IO (B.Builder ())    -- ^ A IO action return a formatted date\/time string
    , loggerFmt             :: LogFormatter
    }

data LoggerConfig = LoggerConfig
    { loggerMinFlushInterval :: {-# UNPACK #-} !Int -- ^ Minimal flush interval, see Notes on 'debug'
    , loggerLineBufSize      :: {-# UNPACK #-} !Int -- ^ Buffer size to build each log line
    , loggerShowDebug        :: Bool                -- ^ Set to 'False' to filter debug logs
    }

-- | A default logger config with
--
-- * 0.1s minimal flush interval
-- * line buffer size 240 bytes
-- * show debug True
defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig 1 240 True

-- | A default timestamp cache with format @%Y-%m-%dT%H:%M:%S%Z@
--
-- The timestamp will updated in 0.1s granularity to ensure a seconds level precision.
defaultTSCache :: IO (B.Builder ())
{-# NOINLINE defaultTSCache #-}
defaultTSCache = unsafePerformIO $ do
    throttle 1 $ do
        t <- getSystemTime
        CB.toBuilder <$> formatSystemTime simpleDateFormat t

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
--
newLogger :: Output o
          => LoggerConfig
          -> MVar (BufferedOutput o)
          -> IO Logger
newLogger LoggerConfig{..} oLock = do
    bList <- newIORef []
    let flush = flushLog oLock bList
    throttledFlush <- throttleTrailing_ loggerMinFlushInterval flush
    return $ Logger (pushLog bList) flush throttledFlush defaultTSCache
        (defaultFmt loggerShowDebug)
  where
    pushLog bList b = do
        let !bs = B.buildBytesWith loggerLineBufSize b
        atomicModifyIORef' bList (\ bss -> (bs:bss, ()))

-- | Make a new colored logger connected to stderr.
--
-- This logger will output colorized log if stderr is connected to TTY.
newColoredLogger :: LoggerConfig -> IO Logger
newColoredLogger LoggerConfig{..} = do
    bList <- newIORef []
    let flush = flushLog stderrBuf bList
    throttledFlush <- throttleTrailing_ loggerMinFlushInterval flush
    return $ Logger (pushLog bList) flush throttledFlush defaultTSCache
        (if isStdStreamTTY stderr then coloredFmt loggerShowDebug
                                  else defaultFmt loggerShowDebug)

  where
    pushLog bList b = do
        let !bs = B.buildBytesWith loggerLineBufSize b
        atomicModifyIORef' bList (\ bss -> (bs:bss, ()))

-- | A default log formatter
--
-- @ [DEBUG][2020-10-09T07:44:14UTC][<interactive>:7:1]This a debug message\\n@
defaultFmt :: Bool  -- ^ show DEGUG?
           -> LogFormatter
defaultFmt showdebug ts level content cstack = when (showdebug || level /= "DEBUG") $ do
    B.square (CB.toBuilder level)
    B.square ts
    B.square $ defaultFmtCallStack cstack
    content
    B.char8 '\n'

-- | A default colored log formatter
--
-- DEBUG level is 'Cyan', WARN level is 'Yellow', FATAL level is 'Red'.
coloredFmt :: Bool  -- ^ show DEBUG?
           -> LogFormatter
coloredFmt showdebug ts level content cstack = when (showdebug || level /= "DEBUG") $ do
    let blevel = CB.toBuilder level
    B.square (case level of
        "DEBUG" -> color Cyan blevel
        "WARN"  -> color Yellow blevel
        "FATAL" -> color Red blevel
        _       -> blevel)
    B.square ts
    B.square $ defaultFmtCallStack cstack
    content
    B.char8 '\n'

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
    newIORef =<< newColoredLogger defaultLoggerConfig

-- | Change the global logger.
setDefaultLogger :: Logger -> IO ()
setDefaultLogger !logger = atomicWriteIORef globalLogger logger

-- | Get the global logger.
getDefaultLogger :: IO Logger
getDefaultLogger = readIORef globalLogger

-- | Manually flush stderr logger.
flushDefaultLogger :: IO ()
flushDefaultLogger = getDefaultLogger >>= flushLogger

-- | Flush stderr logger when program exits.
withDefaultLogger :: IO () -> IO ()
withDefaultLogger = (`finally` flushDefaultLogger)

--------------------------------------------------------------------------------

type Level = CB.CBytes

debug :: HasCallStack => B.Builder () -> IO ()
debug = otherLevel_ "DEBUG" False callStack

info :: HasCallStack => B.Builder () -> IO ()
info = otherLevel_ "INFO" False callStack

warn :: HasCallStack => B.Builder () -> IO ()
warn = otherLevel_ "WARN" False callStack

fatal :: HasCallStack => B.Builder () -> IO ()
fatal = otherLevel_ "FATAL" True callStack

otherLevel :: HasCallStack
           => Level             -- ^ log level
           -> Bool              -- ^ flush immediately?
           -> B.Builder ()      -- ^ log content
           -> IO ()
otherLevel level flushNow bu = otherLevel_ level flushNow callStack bu

otherLevel_ :: Level -> Bool -> CallStack -> B.Builder () -> IO ()
otherLevel_ level flushNow cstack bu = do
    logger <- getDefaultLogger
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
             -> Level             -- ^ log level
             -> Bool              -- ^ flush immediately?
             -> B.Builder ()      -- ^ log content
             -> IO ()
otherLevelTo logger level flushNow =
    otherLevelTo_ level flushNow callStack logger

otherLevelTo_ :: Level -> Bool -> CallStack -> Logger -> B.Builder () -> IO ()
otherLevelTo_ level flushNow cstack logger bu = do
    ts <- loggerTSCache logger
    (loggerPushBuilder logger) $ (loggerFmt logger) ts level bu cstack
    if flushNow
    then flushLogger logger
    else flushLoggerThrottled logger
