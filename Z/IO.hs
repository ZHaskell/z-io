{-|
Module      : Z.IO
Description : IO Umbrella module
Copyright   : (c) Dong Han, 2017
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This re-exports several common module to be used together with file, network, and or specific module,
such as resource management, buffered IO and std streams.

-}

module Z.IO
  ( module Std.IO.Buffered
  , module Std.IO.Environment
  , module Std.IO.Exception
  , module Std.IO.Logger
  , module Std.IO.Resource
  , module Std.IO.StdStream
  ) where

import Std.IO.Buffered
import Std.IO.Environment
import Std.IO.Exception
import Std.IO.Logger
import Std.IO.Resource
import Std.IO.StdStream
