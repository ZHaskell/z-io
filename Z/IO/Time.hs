{-|
Module      : Z.IO.Time
Description : Fast time functions
Copyright   : (c) Dong Han, 2020
              (c) Kazu Yamamoto 2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides functions directly work on 'SystemTime' type, a compact time type from @time@ library.
For advanced time editing, use @time@ library.

-}
module Z.IO.Time
  ( -- * SystemTime
    SystemTime(..), getSystemTime'
    -- * Parsing
  , parseSystemTime, parseSystemTimeGMT
    -- * Formatting
  , formatSystemTime, formatSystemTimeGMT
    -- * Format
  , TimeFormat, simpleDateFormat, iso8061DateFormat, webDateFormat, mailDateFormat
  ) where

import Data.Time.Clock.System
import Data.Word
import Data.Int
import Foreign.C.Types
import Z.Foreign
import Z.Data.CBytes
import Z.IO.UV.FFI_Env
import Z.IO.Exception
import System.IO.Unsafe (unsafePerformIO)


-- | A alternative version of 'getSystemTime'' based on libuv's @uv_gettimeofday@, which also doesn't use pinned allocation.
getSystemTime' :: HasCallStack => IO SystemTime
getSystemTime' = do
    (TimeVal64 s us) <- getTimeOfDay
    return (MkSystemTime s (fromIntegral us * 1000))

-- | <https://man7.org/linux/man-pages/man3/strftime.3.html strftime> time format.
type TimeFormat = CBytes

-- | Simple format @2020-10-16 03:15:29@.
--
-- The value is \"%Y-%m-%d %H:%M:%S\".
-- This should be used with 'formatSystemTime' and 'parseSystemTime'.
simpleDateFormat :: TimeFormat
simpleDateFormat = "%Y-%m-%d %H:%M:%S"

-- | Simple format @2020-10-16T03:15:29@.
--
-- The value is \"%Y-%m-%dT%H:%M:%S%z\".
-- This should be used with 'formatSystemTime' and 'parseSystemTime'.
iso8061DateFormat :: TimeFormat
iso8061DateFormat = "%Y-%m-%dT%H:%M:%S%z"

-- | Format for web (RFC 2616).
--
-- The value is \"%a, %d %b %Y %H:%M:%S GMT\".
-- This should be used with 'formatSystemTimeGMT' and 'parseSystemTimeGMT'.
webDateFormat :: TimeFormat
webDateFormat = "%a, %d %b %Y %H:%M:%S GMT"

-- | Format for e-mail (RFC 5322).
--
-- The value is \"%a, %d %b %Y %H:%M:%S %z\".
-- This should be used with 'formatSystemTime' and 'parseSystemTime'.
mailDateFormat :: TimeFormat
mailDateFormat = "%a, %d %b %Y %H:%M:%S %z"

----------------------------------------------------------------
-- | Formatting 'SystemTime' to 'CBytes' in local time.
--
-- This is a wrapper for strftime_l(), 'systemNanoseconds' is ignored.
-- The result depends on the TZ environment variable.
--
formatSystemTime :: TimeFormat -> SystemTime -> IO CBytes
formatSystemTime fmt t = formatSystemTimeHelper c_format_unix_time fmt t
{-# INLINE formatSystemTime #-}

-- | Formatting 'SystemTime' to 'CBytes' in GMT.
--
-- This is a wrapper for strftime_l(), 'systemNanoseconds' is ignored.
--
-- >>> formatSystemTimeGMT webDateFormat $ MkSystemTime 0 0
-- "Thu, 01 Jan 1970 00:00:00 GMT"
-- >>> let ut = MkSystemTime 100 200
-- >>> let str = formatSystemTimeGMT "%s" ut
-- >>> let ut' = parseSystemTimeGMT "%s" str
-- >>> ((==) `on` systemSeconds) ut ut'
-- True
-- >>> ((==) `on` systemNanoseconds) ut ut'
-- False
--
formatSystemTimeGMT :: TimeFormat -> SystemTime -> CBytes
formatSystemTimeGMT fmt t =
    unsafePerformIO $ formatSystemTimeHelper c_format_unix_time_gmt fmt t
{-# INLINE formatSystemTimeGMT #-}

----------------------------------------------------------------
-- | Parsing 'CBytes' to 'SystemTime' interpreting as localtime.
--
-- This is a wrapper for strptime_l().
-- Many implementations of strptime_l() do not support %Z and
-- some implementations of strptime_l() do not support %z, either.
-- 'systemNanoSeconds' is always set to 0.
--
-- The result depends on the TZ environment variable.
--
-- @
-- > setEnv "TZ" "Africa\/Algiers"
-- parseSystemTime simpleDateFormat "1970-01-01 00:00:00"
-- MkSystemTime {systemSeconds = 0, systemNanoseconds = 0}
-- > setEnv "TZ" "Asia\/Shanghai"
-- parseSystemTime simpleDateFormat "1970-01-01 00:00:00"
-- MkSystemTime {systemSeconds = -28800, systemNanoseconds = 0}
-- @
--
parseSystemTime :: TimeFormat -> CBytes -> IO SystemTime
parseSystemTime fmt str =
    withCBytesUnsafe fmt $ \cfmt ->
        withCBytesUnsafe str $ \cstr -> do
            sec <- c_parse_unix_time cfmt cstr
            return $ MkSystemTime sec 0

-- | Parsing 'CBytes' to 'SystemTime' interpreting as GMT.
-- This is a wrapper for strptime_l().
-- 'systemNanoSeconds' is always set to 0.
--
-- >>> parseSystemTimeGMT webDateFormat "Thu, 01 Jan 1970 00:00:00 GMT"
-- MkSystemTime {systemSeconds = 0, systemNanoseconds = 0}

parseSystemTimeGMT :: TimeFormat -> CBytes -> SystemTime
parseSystemTimeGMT fmt str = unsafePerformIO $
    withCBytesUnsafe fmt $ \cfmt ->
        withCBytesUnsafe str $ \cstr -> do
            sec <- c_parse_unix_time_gmt cfmt cstr
            return $ MkSystemTime sec 0

--------------------------------------------------------------------------------

foreign import ccall unsafe "c_parse_unix_time"
        c_parse_unix_time :: BA# Word8 -> BA# Word8 -> IO Int64

foreign import ccall unsafe "c_parse_unix_time_gmt"
        c_parse_unix_time_gmt :: BA# Word8 -> BA# Word8 -> IO Int64

foreign import ccall unsafe "c_format_unix_time"
        c_format_unix_time :: BA# Word8 -> Int64 -> MBA# Word8 -> CInt -> IO CSize

foreign import ccall unsafe "c_format_unix_time_gmt"
        c_format_unix_time_gmt :: BA# Word8 -> Int64 -> MBA# Word8 -> CInt -> IO CSize

-- | Helper handling memory allocation for formatSystemTime and formatSystemTimeGMT.
formatSystemTimeHelper
    :: (BA# Word8 -> Int64 -> MBA# Word8 -> CInt -> IO CSize)
    -> TimeFormat
    -> SystemTime
    -> IO CBytes
formatSystemTimeHelper formatFun fmt t = go 80
  where
    MkSystemTime sec _ = t
    go !siz = do
        (bs, r)<- allocCBytesUnsafe siz $ \ pbuf ->
            withCBytesUnsafe fmt $ \ pfmt ->
                formatFun pfmt sec pbuf (fromIntegral siz)
        if r <= 0 then go (siz*2) else return bs
