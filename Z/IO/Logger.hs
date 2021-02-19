{-# LANGUAGE LambdaCase #-}

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

Flushing is automatic and throttled for 'debug', 'info', 'warning' to boost
performance, but a 'fatal' and 'critical' log will always flush logger's buffer.
This could lead to a problem that if main thread exits too early logs may missed,
to add a flushing when program exits, use 'withDefaultLogger' like:

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
    Logger
  , LoggerConfig(..)
  , defaultLoggerConfig
  , defaultJSONLoggerConfig
  , setDefaultLogger
  , getDefaultLogger
  , flushDefaultLogger
  , withDefaultLogger

    -- * Create a new logger
  , newLogger
  , newStdLogger
  , newFileLogger

    -- * logging functions
  , debug
  , info
  , warning
  , fatal
  , critical
  , otherLevel

    -- * logging functions with specific logger
  , debugTo
  , infoTo
  , warningTo
  , fatalTo
  , otherLevelTo

    -- * Helpers to write new log formatter
  , LogFormatter, defaultFmt, defaultColoredFmt, defaultJSONFmt
  , defaultFmtCallStack
  , defaultLevelFmt

    -- * Constants
    -- ** Level
  , Level
  , pattern DEBUG
  , pattern INFO
  , pattern WARNING
  , pattern FATAL
  , pattern CRITICAL
  , pattern NOTSET
  ) where

import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Bits               ((.|.))
import           Data.IORef
import           Foreign.C.Types         (CInt (..))
import           GHC.Conc.Sync           (ThreadId (..), myThreadId)
import           GHC.Exts                (ThreadId#)
import           GHC.Stack
import           System.IO.Unsafe        (unsafePerformIO)
import qualified Z.Data.Builder          as B
import qualified Z.Data.CBytes           as CB
import qualified Z.Data.JSON.Builder     as JB
import           Z.Data.Vector.Base      as V
import           Z.IO.Buffered
import           Z.IO.Exception
import qualified Z.IO.FileSystem         as ZF
import           Z.IO.LowResTimer
import           Z.IO.Resource
import           Z.IO.StdStream
import           Z.IO.StdStream.Ansi     (AnsiColor (..), color)
import           Z.IO.Time

-------------------------------------------------------------------------------

type LogFormatter = B.Builder ()            -- ^ data\/time string(second precision)
                  -> Level                  -- ^ log level
                  -> B.Builder ()           -- ^ log content
                  -> CallStack              -- ^ call stack trace
                  -> ThreadId               -- ^ logging thread id
                  -> B.Builder ()

-- | Extensible logger type.
data Logger = Logger
    { loggerPushBuilder    :: B.Builder () -> IO ()
    -- ^ Push log into buffer
    , flushLogger          :: IO ()
    -- ^ Flush logger's buffer to output device
    , flushLoggerThrottled :: IO ()
    -- ^ Throttled flush, e.g. use 'throttleTrailing_' from "Z.IO.LowResTimer"
    , loggerTSCache        :: IO (B.Builder ())
    -- ^ An IO action return a formatted date\/time string
    , loggerFmt            :: LogFormatter
    -- ^ Log formatter
    , loggerLevel          :: {-# UNPACK #-} !Level
    -- ^ Output logs if level is equal or higher than this value.
    }

-- | Logger config type used in this module.
data LoggerConfig = LoggerConfig
    { loggerMinFlushInterval :: {-# UNPACK #-} !Int
    -- ^ Minimal flush interval, see Notes on 'debug'
    , loggerLineBufSize      :: {-# UNPACK #-} !Int
    -- ^ Buffer size to build each log line
    , loggerConfigLevel      :: {-# UNPACK #-} !Level
    -- ^ Config log's filter level
    , loggerFormatter        :: LogFormatter
    -- ^ Log formatter
    }

-- | A default logger config with
--
-- * 0.1s minimal flush interval
-- * line buffer size 240 bytes
-- * show everything by default
-- * 'defaultFmt'
defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig 1 240 NOTSET defaultFmt

-- | A default logger config with
--
-- * 0.5s minimal flush interval
-- * line buffer size 1000 bytes
-- * show everything by default
-- * 'defaultJSONFmt'
defaultJSONLoggerConfig :: LoggerConfig
defaultJSONLoggerConfig = LoggerConfig 5 1000 NOTSET defaultJSONFmt

-- | A default timestamp cache with format @%Y-%m-%dT%H:%M:%S%Z@('iso8061DateFormat').
--
-- The timestamp will updated in 0.1s granularity to ensure a seconds level precision.
defaultTSCache :: IO (B.Builder ())
{-# NOINLINE defaultTSCache #-}
defaultTSCache = unsafePerformIO $ do
    throttle 1 $ do
        t <- getSystemTime'
        CB.toBuilder <$> formatSystemTime iso8061DateFormat t

-------------------------------------------------------------------------------

-- | Make a new logger with given write device.
newLogger :: LoggerConfig
          -> MVar BufferedOutput
          -> IO Logger
newLogger LoggerConfig{..} oLock = do
    logsRef <- newIORef []
    let flush = flushLogIORef oLock logsRef
    throttledFlush <- throttleTrailing_ loggerMinFlushInterval flush
    return $ Logger (pushLogIORef logsRef loggerLineBufSize)
                    flush throttledFlush defaultTSCache loggerFormatter
                    loggerConfigLevel

-- | Make a new logger write to 'stderrBuf'.
newStdLogger :: LoggerConfig -> IO Logger
newStdLogger config = newLogger config stderrBuf

-- | Make a new file based logger with 'defaultFmt'.
--
-- The file will be opened in append mode.
newFileLogger :: LoggerConfig -> CB.CBytes -> IO Logger
newFileLogger config path = do
    let res = ZF.initFile path (ZF.O_CREAT .|. ZF.O_RDWR .|. ZF.O_APPEND) ZF.DEFAULT_FILE_MODE
    (file, _closeFunc) <- acquire res
    oLock <- newMVar =<< newBufferedOutput file
    newLogger config oLock

-------------------------------------------------------------------------------

-- | Use 'pushLogIORef' and 'pushLogIORef' to implement a simple 'IORef' based concurrent logger.
--
-- @
-- logsRef <- newIORef []
-- let push = pushLogIORef logsRef lineBufSize
--     flush = flushLogIORef stderrBuf logsRef
--     throttledFlush <- throttleTrailing_ flushInterval flush
-- ..
-- return $ Logger push flush throttledFlush ...
-- @
--
pushLogIORef :: IORef [V.Bytes]     -- ^ logs stored in a list, new log will be CASed into it.
             -> Int                 -- ^ buffer size to build each log
             -> B.Builder ()        -- ^ formatted log
             -> IO ()
pushLogIORef logsRef loggerLineBufSize b = do
    let !bs = B.buildWith loggerLineBufSize b
    unless (V.null bs) $ atomicModifyIORef' logsRef (\ bss -> (bs:bss, ()))

flushLogIORef :: HasCallStack => MVar BufferedOutput -> IORef [V.Bytes] -> IO ()
flushLogIORef oLock logsRef =
    withMVar oLock $ \ o -> do
        bss <- atomicModifyIORef' logsRef (\ bss -> ([], bss))
        forM_ (reverse bss) (writeBuffer o)
        flushBuffer o

-- | A default log formatter
--
-- @[FATAL][2021-02-01T15:03:30+0800][<interactive>:31:1][thread#669]...@
defaultFmt :: LogFormatter
defaultFmt ts level content cstack (ThreadId tid#) = do
    B.square (defaultLevelFmt level)
    B.square ts
    B.square $ defaultFmtCallStack cstack
    B.square $ "thread#" >> B.int (getThreadId tid#)
    content
    B.char8 '\n'

-- | A default colored log formatter
--
-- DEBUG level is 'Cyan', WARNING level is 'Yellow', FATAL and CRITICAL level are 'Red'.
defaultColoredFmt :: LogFormatter
defaultColoredFmt ts level content cstack (ThreadId tid#) = do
    let blevel = defaultLevelFmt level
    B.square (case level of
        DEBUG    -> color Cyan blevel
        WARNING  -> color Yellow blevel
        FATAL    -> color Red blevel
        CRITICAL -> color Red blevel
        _        -> blevel)
    B.square ts
    B.square $ defaultFmtCallStack cstack
    B.square $ "thread#" >> B.int (getThreadId tid#)
    content
    B.char8 '\n'

-- | A default JSON log formatter.
--
-- > {"level":"FATAL","time":"2021-02-01T15:02:19+0800","loc":"<interactive>:27:1","theadId":606,"content":"..."}\n
defaultJSONFmt :: LogFormatter
defaultJSONFmt ts level content cstack (ThreadId tid#) = do
    B.curly $ do
        "level" `JB.kv` B.quotes (defaultLevelFmt level)
        B.comma
        "time" `JB.kv` B.quotes ts
        B.comma
        "loc" `JB.kv` B.quotes (defaultFmtCallStack cstack)
        B.comma
        "thead" `JB.kv`  B.int (getThreadId tid#)
        B.comma
        "content" `JB.kv` JB.string (B.unsafeBuildText content)
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
    newIORef =<< newStdLogger defaultLoggerConfig{
        loggerFormatter = (if isStdStreamTTY stderr then defaultColoredFmt else defaultFmt)
    }

-- | Change the global logger.
setDefaultLogger :: Logger -> IO ()
setDefaultLogger !logger = atomicWriteIORef globalLogger logger

-- | Get the global logger.
--
-- This is a logger connected to stderr, if stderr is connect to TTY,
-- then use 'defaultColoredFmt', otherwise use 'defaultFmt'.
getDefaultLogger :: IO Logger
getDefaultLogger = readIORef globalLogger

-- | Manually flush global logger.
flushDefaultLogger :: IO ()
flushDefaultLogger = getDefaultLogger >>= flushLogger

-- | Flush global logger when program exits.
withDefaultLogger :: IO () -> IO ()
withDefaultLogger = (`finally` flushDefaultLogger)

--------------------------------------------------------------------------------

-- | Logging Levels
--
-- We following the Python logging levels, for details,
-- see: <https://docs.python.org/3/howto/logging.html#logging-levels>
--
-- +----------+---------------+
-- | Level    | Numeric value |
-- +----------+---------------+
-- | CRITICAL | 50            |
-- +----------+---------------+
-- | FATAL    | 40            |
-- +----------+---------------+
-- | WARNING  | 30            |
-- +----------+---------------+
-- | INFO     | 20            |
-- +----------+---------------+
-- | DEBUG    | 10            |
-- +----------+---------------+
-- | NOTSET   | 0             |
-- +----------+---------------+
--
type Level = Int

pattern CRITICAL :: Level
pattern CRITICAL = 50

pattern FATAL :: Level
pattern FATAL = 40

pattern WARNING :: Level
pattern WARNING = 30

pattern INFO :: Level
pattern INFO = 20

pattern DEBUG :: Level
pattern DEBUG = 10

pattern NOTSET :: Level
pattern NOTSET = 0

-- | Format 'DEBUG' to 'DEBUG', etc.
--
-- Level other than built-in ones, are formatted in decimal numeric format, i.e.
-- @defaultLevelFmt 60 == "LEVEL60"@
defaultLevelFmt :: Level -> B.Builder ()
defaultLevelFmt level = case level of
    CRITICAL -> "CRITICAL"
    FATAL    -> "FATAL"
    WARNING  -> "WARNING"
    INFO     -> "INFO"
    DEBUG    -> "DEBUG"
    NOTSET   -> "NOTSET"
    level'   -> "LEVEL" >> B.int level'

debug :: HasCallStack => B.Builder () -> IO ()
debug = otherLevel_ DEBUG False callStack

info :: HasCallStack => B.Builder () -> IO ()
info = otherLevel_ INFO False callStack

warning :: HasCallStack => B.Builder () -> IO ()
warning = otherLevel_ WARNING False callStack

fatal :: HasCallStack => B.Builder () -> IO ()
fatal = otherLevel_ FATAL True callStack

critical :: HasCallStack => B.Builder () -> IO ()
critical = otherLevel_ CRITICAL True callStack

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
debugTo = otherLevelTo_ DEBUG False callStack

infoTo :: HasCallStack => Logger -> B.Builder () -> IO ()
infoTo = otherLevelTo_ INFO False callStack

warningTo :: HasCallStack => Logger -> B.Builder () -> IO ()
warningTo = otherLevelTo_ WARNING False callStack

fatalTo :: HasCallStack => Logger -> B.Builder () -> IO ()
fatalTo = otherLevelTo_ FATAL True callStack

otherLevelTo :: HasCallStack
             => Logger
             -> Level             -- ^ log level
             -> Bool              -- ^ flush immediately?
             -> B.Builder ()      -- ^ log content
             -> IO ()
otherLevelTo logger level flushNow =
    otherLevelTo_ level flushNow callStack logger

otherLevelTo_ :: Level -> Bool -> CallStack -> Logger -> B.Builder () -> IO ()
otherLevelTo_ level flushNow cstack logger bu = when (level >= loggerLevel logger) $ do
    ts <- loggerTSCache logger
    tid <- myThreadId
    (loggerPushBuilder logger) $ (loggerFmt logger) ts level bu cstack tid
    if flushNow
    then flushLogger logger
    else flushLoggerThrottled logger

foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> CInt
