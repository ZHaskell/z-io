{-|
Module      : Z.IO.StdStream.Ansi
Description : Ansi control code sequences
Copyright   : (c) Winterland, 2017-2020
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : experimental
Portability : non-portable

Provides utilities

@
@

-}

module Z.IO.StdStream.ReadLine
  ( -- * Style modifier
  ) where


data ReadLine = ReadLine
    { promptText :: IORef T.Text
    , keyBinding ::
    , event :: STM Event
    ,

    }

initReaderLine :: ReadLineConfig -> Resource ReadLine
initReaderLine = do






