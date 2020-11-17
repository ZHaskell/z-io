module Z.IO.FileSystem.Watch where

import Z.IO.FileSystem
import Z.IO.BIO
import Z.IO.BIO.Concurrent
import Z.Data.CBytes

data FileEvent
    = FileAdd CBytes FStat
    | FileRemove CBytes FStat
    | FileMove CBytes FStat CBytes FStat


-- | Start watch a given file or directory.
--
--
watchFiles :: CBytes -> IO (Source FileEvent)
watchFiles dir =

