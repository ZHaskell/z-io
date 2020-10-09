#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
module Z.IO.FileSystem.FilePath(module Z.IO.FileSystem.FilePathWin) where
import Z.IO.FileSystem.FilePathWin
#else
module Z.IO.FileSystem.FilePath(module Z.IO.FileSystem.FilePathPosix) where
import Z.IO.FileSystem.FilePathPosix
#endif
