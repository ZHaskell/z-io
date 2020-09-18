{-# LANGUAGE CPP #-}
{-|
Module      : Z.IO.UV.Win
Description : Special code on windows
Copyright   : (c) Winterland, 2017-2018
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable
-}
module Z.IO.UV.Win where

import System.IO.Unsafe
import Control.Exception

-- | 'withUVInitDo' is necessary for some socket code because on windows WSAStartup has to be called
-- before use sockets.
--
-- This functions will run 'uv__once_init' once if not run before,
--
withUVInitDo :: IO a -> IO a
{-# INLINE withUVInitDo #-}

#if defined(mingw32_HOST_OS)

withUVInitDo act = evaluate withUVInit >> act

{-# NOINLINE withUVInit #-}
withUVInit :: ()
-- Use a CAF to make forcing it do initialisation once, but subsequent forces will be cheap
withUVInit = unsafePerformIO $ uv__once_init

foreign import ccall unsafe "uv__once_init" uv__once_init :: IO ()
#else

withUVInitDo x = x
#endif
