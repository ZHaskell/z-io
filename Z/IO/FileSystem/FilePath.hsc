{-|
Module      : Z.IO.FileSystem.FilePath
Description : file path toolbox
Copyright   : (c) Dong Han, 2017~2019
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides file path manipulations using <https://likle.github.io/cwalk/ cwalk>,
both unix and window style path is accepted. Default style is choosen during compile time,
but can be changed during runtime.
-}

module Z.IO.FileSystem.FilePath
 ( -- * Paths
    splitBaseName, changeBaseName
  , splitRoot, changeRoot
  , isAbsolute
  , isRelative
  , join
  , concat
  , normalize
  , intersection
  , absolute
  , relative
  -- * Extensions
  , splitExtension, changeExtension
  -- * Path Style
  , PathStyle(..)
  , pathStyle
  , getPathStyle, setPathStyle
  , pathSeparator, searchPathSeparator, extensionSeparator
  -- * Search path
  , getSearchPath
 ) where


import           Data.Word
import qualified Data.List as List
import           GHC.Generics
import           Z.Data.CBytes (CBytes(CB), allocCBytesUnsafe, withCBytesUnsafe, withCBytesListUnsafe)
import qualified Z.Data.CBytes as CB
import qualified Z.Data.Text.ShowT as T
import           Z.Data.JSON (FromValue, ToValue, EncodeJSON)
import qualified Z.Data.Vector.Base as V
import qualified Z.Data.Vector      as V
import           Z.Foreign
import           Z.IO.Environment   (getEnv')
import Prelude hiding (concat)

#include "hs_cwalk.h"
-- \
#define BACKSLASH 92
-- /
#define SLASH 47
-- "
#define DOUBLE_QUOTE 34
-- :
#define COLON 58
-- ;
#define SEMICOLON 59
-- .
#define DOT 46

#define BUF_EXT_SIZ  4

data PathStyle = WindowsStyle   -- ^ Use backslashes as a separator and volume for the root.
               | UnixStyle      -- ^ Use slashes as a separator and a slash for the root.
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (T.ShowT, FromValue, ToValue, EncodeJSON)

enumToPathStyle_ :: CInt -> PathStyle
enumToPathStyle_ (#const CWK_STYLE_WINDOWS) = WindowsStyle
enumToPathStyle_ _ = UnixStyle

pathStyleToEnum_ :: PathStyle -> CInt
pathStyleToEnum_ WindowsStyle = (#const CWK_STYLE_WINDOWS)
pathStyleToEnum_ _ = (#const CWK_STYLE_UNIX)

-- | Guesses the path style.
--
-- This function guesses the path style based on a submitted path-string.
-- The guessing will look at the root and the type of slashes contained in the path
-- and return the style which is more likely used in the path. The algorithm checks the following:
--
-- * If the root is longer than 1 character      -> WINDOWS
-- * If the first separator is a backslash       -> WINDOWS
-- * If the first separator is a slash           -> UNIX
-- * If the last segment starts with a dot       -> UNIX
-- * If the last segment contains a dot          -> WINDOWS
-- * If nothing was found to determine the style -> UNIX
--
pathStyle :: CBytes -> IO PathStyle
pathStyle p = enumToPathStyle_ <$> withCBytesUnsafe p cwk_path_guess_style

-- | Gets the path style currently using.
getPathStyle :: IO PathStyle
getPathStyle = enumToPathStyle_ <$> cwk_path_get_style

-- | Configures which path style is used afterwards.
--
-- This function configures which path style is used.
-- call to this function is only required if a non-native behaviour is required. 
-- The style defaults to 'WindowsStyle' on windows builds and to 'UnixStyle' otherwise.
setPathStyle :: PathStyle -> IO ()
setPathStyle = cwk_path_set_style . pathStyleToEnum_

-- | Get the character that separates directories. 
pathSeparator :: IO [Word8]
pathSeparator = do
    s <- getPathStyle
    case s of UnixStyle -> return [(#const SLASH)]
              _         -> return [(#const SLASH), (#const BACKSLASH)]

-- | The character that is used to separate the entries in the $PATH environment variable.
--
-- * Windows: searchPathSeparator is ASCII @;@
-- * Unix:    searchPathSeparator is ASCII @:@
--
searchPathSeparator :: IO Word8
searchPathSeparator = do
    s <- getPathStyle
    case s of UnixStyle -> return (#const COLON)
              _         -> return (#const SEMICOLON)

-- | File extension character
--
-- ExtSeparator is ASCII @.@
extensionSeparator :: Word8
extensionSeparator = #const DOT

-- | Get the basename of a file path.
--
-- The basename is the last segment of a path. For instance, @logs@ is the basename of the path @\/var\/logs@.
--
-- +--------------------------+---------------+
-- |     Path                 |   Basename    |
-- +--------------------------+---------------+
-- | \/my\/path.txt           | path.txt      |
-- +--------------------------+---------------+
-- | \/my\/path.txt\/         | path.txt      |
-- +--------------------------+---------------+
-- | \/my\/path.txt\/\/\/\/   | path.txt      |
-- +--------------------------+---------------+
-- | file_name                | file_name     |
-- +--------------------------+---------------+
-- | ..                       | ..            |
-- +--------------------------+---------------+
-- | .                        | .             |
-- +--------------------------+---------------+
-- | \/                       | ""            |
-- +--------------------------+---------------+
-- | C:\path\test.txt         | test.txt      |
-- +--------------------------+---------------+
--
splitBaseName :: CBytes
              -> IO (CBytes, CBytes)    -- ^ return (dir, basename)
{-# INLINABLE splitBaseName #-}
splitBaseName p = do
    (off, len) <- withCBytesUnsafe p $ \ pp ->
        allocPrimUnsafe $ \ poff ->
        hs_cwk_path_get_basename pp poff
    if len == 0
    then return (p, CB.empty)
    else return ( CB (V.PrimVector (CB.rawPrimArray p) 0 off)
                , CB (V.PrimVector (CB.rawPrimArray p) off len))


-- | Changes the basename of a file path.
--
-- @
-- > changeBaseName  "foo\/bar.txt" "qux.png"
-- "foo\/qux.png"
-- @
--
changeBaseName :: CBytes
               -> CBytes   -- ^ new base name
               -> IO CBytes
changeBaseName p b = do
    let l = CB.length p + CB.length b + (#const BUF_EXT_SIZ)
    (p', _) <- withCBytesUnsafe p $ \ pp ->
        withCBytesUnsafe b $ \ pb ->
            allocCBytesUnsafe l $ \ pbuf ->
                cwk_path_change_basename pp pb pbuf (fromIntegral l)
    return p'

-- | Determines the root of a path.
--
-- This function determines the root of a path by finding it’s length.
-- The root comes before the first segment of the path.
-- For example, @C:\\@ is the root of @C:\\folder\\file.txt@.
-- It always starts at the submitted path. If the path has no root, 'CB.empty' will be returned.
--
-- +---------+--------------------------+----------------------+
-- | Style   | Path                     | Root                 |
-- +---------+--------------------------+----------------------+
-- | UNIX    | \/test\/                 | \/                   |
-- +---------+--------------------------+----------------------+
-- | UNIX    | test.txt                 | ""                   |
-- +---------+--------------------------+----------------------+
-- | UNIX    | C:\\test.txt             | ""                   |
-- +---------+--------------------------+----------------------+
-- | UNIX    | \\folder\\               | ""                   |
-- +---------+--------------------------+----------------------+
-- | WINDOWS | \/test.txt               | \/                   |
-- +---------+--------------------------+----------------------+
-- | WINDOWS | \\test.txt               | \\                   |
-- +---------+--------------------------+----------------------+
-- | WINDOWS | C:\\test.txt             | C:\\                 |
-- +---------+--------------------------+----------------------+
-- | WINDOWS | \\\\server\\folder\\data | \\\\server\\folder\\ |
-- +---------+--------------------------+----------------------+
-- | WINDOWS | \\\\.\\folder\\data      | \\\\.\\              |
-- +---------+--------------------------+----------------------+
-- | WINDOWS | \\\\?\\folder\\data      | \\\\?\\              |
-- +---------+--------------------------+----------------------+
-- | WINDOWS | C:test.txt               | C:                   |
-- +---------+--------------------------+----------------------+
-- | WINDOWS | ..\\hello\\world.txt     | ""                   |
-- +---------+--------------------------+----------------------+
--
splitRoot :: CBytes 
          -> IO (CBytes, CBytes) -- ^ return (root, rest path)
{-# INLINABLE splitRoot #-}
splitRoot p = do
    off <- withCBytesUnsafe p hs_cwk_path_get_root
    if off == 0
    then return (CB.empty, p)
    else return ( CB (V.PrimVector (CB.rawPrimArray p) 0 off)
                , CB (V.PrimVector (CB.rawPrimArray p) off (CB.length p - off)))

-- | Changes the root of a file path.
--
-- @
-- > changeBaseName "C:\\\\test.txt" "D:\\\\"    -- windows style
-- "D:\\test.txt"
-- @
--
changeRoot :: CBytes
           -> CBytes   -- ^ new base name
           -> IO CBytes
changeRoot p r = do
    let l = CB.length p + CB.length r + (#const BUF_EXT_SIZ)
    (p', _) <- withCBytesUnsafe p $ \ pp ->
        withCBytesUnsafe r $ \ pr ->
            allocCBytesUnsafe l $ \ pbuf ->
                cwk_path_change_root pp pr pbuf (fromIntegral l)
    return p'

-- | Determine whether the path is absolute or not.
--
-- This function checks whether the path is an absolute (fully qualified) path or not.
-- A path is considered to be absolute if the root ends with a separator.
--
-- +---------+--------------------------+-----------+
-- | Style   | Path                     | Result    |
-- +---------+--------------------------+-----------+
-- | UNIX    | \/test\/                 | True      |
-- +---------+--------------------------+-----------+
-- | UNIX    | test.txt                 | False     |
-- +---------+--------------------------+-----------+
-- | UNIX    | C:\\test.txt             | False     |
-- +---------+--------------------------+-----------+
-- | UNIX    | \\folder\\               | False     |
-- +---------+--------------------------+-----------+
-- | WINDOWS | \/test.txt               | True      |
-- +---------+--------------------------+-----------+
-- | WINDOWS | \\test.txt               | True      |
-- +---------+--------------------------+-----------+
-- | WINDOWS | C:\\test.txt             | True      |
-- +---------+--------------------------+-----------+
-- | WINDOWS | \\\\server\\folder\\data | True      |
-- +---------+--------------------------+-----------+
-- | WINDOWS | \\\\.\\folder\\data      | True      |
-- +---------+--------------------------+-----------+
-- | WINDOWS | \\\\?\\folder\\data      | True      |
-- +---------+--------------------------+-----------+
-- | WINDOWS | C:test.txt               | False     |
-- +---------+--------------------------+-----------+
-- | WINDOWS | ..\\hello\\world.txt     | False     |
-- +---------+--------------------------+-----------+
--
isAbsolute :: CBytes -> IO Bool
isAbsolute p = (/=0) <$> withCBytesUnsafe p cwk_path_is_absolute

-- | Determine whether the path is relative or not.
--
-- This function checks whether the path is a relative path or not.
-- A path is considered to be relative if the root does not end with a separator.
--
-- +---------+--------------------------+-----------+
-- | Style   | Path                     | Result    |
-- +---------+--------------------------+-----------+
-- | UNIX    | \/test\/                 | False     |
-- +---------+--------------------------+-----------+
-- | UNIX    | test.txt                 | True      |
-- +---------+--------------------------+-----------+
-- | UNIX    | C:\\test.txt             | True      |
-- +---------+--------------------------+-----------+
-- | UNIX    | \\folder\\               | True      |
-- +---------+--------------------------+-----------+
-- | WINDOWS | \/test.txt               | False     |
-- +---------+--------------------------+-----------+
-- | WINDOWS | \\test.txt               | False     |
-- +---------+--------------------------+-----------+
-- | WINDOWS | C:\\test.txt             | False     |
-- +---------+--------------------------+-----------+
-- | WINDOWS | \\\\server\\folder\\data | False     |
-- +---------+--------------------------+-----------+
-- | WINDOWS | \\\\.\\folder\\data      | False     |
-- +---------+--------------------------+-----------+
-- | WINDOWS | \\\\?\\folder\\data      | False     |
-- +---------+--------------------------+-----------+
-- | WINDOWS | C:test.txt               | True      |
-- +---------+--------------------------+-----------+
-- | WINDOWS | ..\\hello\\world.txt     | True      |
-- +---------+--------------------------+-----------+
--
isRelative :: CBytes -> IO Bool
isRelative p = (/=0) <$> withCBytesUnsafe p cwk_path_is_relative

-- | Joins two paths together.
--
-- +---------+-----------------------+---------------------------+--------------------------------------+
-- | Style   | Path A                | Path B                    | Result                               |
-- +---------+-----------------------+---------------------------+--------------------------------------+
-- | UNIX    | hello\/there          | ..\/world                 | hello\/world                         |
-- +---------+-----------------------+---------------------------+--------------------------------------+
-- | UNIX    | \/first               | \/second                  | \/first\/second                      |
-- +---------+-----------------------+---------------------------+--------------------------------------+
-- | UNIX    | hello                 | ..                        | .                                    |
-- +---------+-----------------------+---------------------------+--------------------------------------+
-- | UNIX    | hello\/there          | ..                        | hello                                |
-- +---------+-----------------------+---------------------------+--------------------------------------+
-- | UNIX    | hello                 | there                     | hello\/there                         |
-- +---------+-----------------------+---------------------------+--------------------------------------+
-- | WINDOWS | this\\                | C:\\..\\..\\is\\a\\test\\ | is\\a\\test                          |
-- +---------+-----------------------+---------------------------+--------------------------------------+
-- | WINDOWS | C:\\this\\path        | C:\\is\\a\\test\\         | C:\\this\\path\\C:\\is\\a\\test      |
-- +---------+-----------------------+---------------------------+--------------------------------------+
-- | WINDOWS | C:\\this\\path        | C:\\..\\is\\a\\test\\     | C:\\this\\path\\is\\a\\test          |
-- +---------+-----------------------+---------------------------+--------------------------------------+
-- | WINDOWS | \\\\s1\\unc\\path     | \\\\s2\\unc\\pa           | \\\\s1\\unc\\pa\\s2\\unc\\path       |
-- +---------+-----------------------+---------------------------+--------------------------------------+
--
join :: CBytes -> CBytes -> IO CBytes
join p p2 = do
    let l = CB.length p + CB.length p2 + (#const BUF_EXT_SIZ)
    (p', _) <- withCBytesUnsafe p $ \ pp ->
        withCBytesUnsafe p2 $ \ pp2 ->
            allocCBytesUnsafe l $ \ pbuf ->
                cwk_path_join pp pp2 pbuf (fromIntegral l)
    return p'

-- | Joins multiple paths together.
--
-- This function generates a new path by joining multiple paths together.
-- It will remove double separators, and unlike 'getAbsolute',
-- it permits the use of multiple relative paths to combine.
concat :: [CBytes] -> IO CBytes
concat ps = do
    (p', _) <- withCBytesListUnsafe ps $ \ pp l -> do
        let l' = sum (List.map (\ p -> CB.length p + (#const BUF_EXT_SIZ)) ps)
        allocCBytesUnsafe l' $ \ pbuf ->
            hs_cwk_path_join_multiple pp l pbuf (fromIntegral l')
    return p'

-- | Creates a normalized version of the path.
-- The following will be true for the normalized path:
--
-- * "..\/" will be resolved.
-- * ".\/" will be removed.
-- * double separators will be fixed with a single separator.
-- * separator suffixes will be removed.
--
-- +--------------------------------------------------+-------------------+
-- | Input                                            | Output            |
-- +--------------------------------------------------+-------------------+
-- | \/var                                            | \/var             |
-- +--------------------------------------------------+-------------------+
-- | \/var\/logs\/test\/..\/..\/                      | \/var             |
-- +--------------------------------------------------+-------------------+
-- | \/var\/logs\/test\/..\/..\/..\/..\/..\/..\/      | \/                |
-- +--------------------------------------------------+-------------------+
-- | rel\/..\/..\/                                    | ..                |
-- +--------------------------------------------------+-------------------+
-- | \/var\/\/\/\/logs\/\/test\/                      | \/var\/logs\/test |
-- +--------------------------------------------------+-------------------+
-- | \/var\/.\/.\/.\/.\/                              | \/var             |
-- +--------------------------------------------------+-------------------+
-- | \/var\/.\/logs\/.\/\/test\/..\/\/..\/\/\/\/\/\/  | \/var             |
-- +--------------------------------------------------+-------------------+
--
normalize :: CBytes -> IO CBytes
normalize p = do
    let l = CB.length p + (#const BUF_EXT_SIZ)
    (p', _) <- withCBytesUnsafe p $ \ pp ->
        allocCBytesUnsafe l $ \ pbuf ->
            cwk_path_normalize pp pbuf (fromIntegral l)
    return p'

-- | Finds common portions in two paths.
--
-- +---------+---------------------------+---------------------------+----------------------+
-- | Style   | Base                      | Other                     | Result               |
-- +---------+---------------------------+---------------------------+----------------------+
-- | UNIX    | \/test\/abc\/..\/foo\/bar | \/test\/foo\/har          | \/test\/abc\/..\/foo |
-- +---------+---------------------------+---------------------------+----------------------+
-- | UNIX    | \/test\/foo\/har          | \/test\/abc\/..\/foo\/bar | \/test\/foo          |
-- +---------+---------------------------+---------------------------+----------------------+
-- | UNIX    | \/test\/abc.txt           | test\/abc.txt             | ""                   |
-- +---------+---------------------------+---------------------------+----------------------+
-- | UNIX    | \/                        | ""                        | ""                   |
-- +---------+---------------------------+---------------------------+----------------------+
-- | UNIX    | \/this\/\/\/is\/a\/\/test | \/this\/\/is\/a\/\/\/file | \/this\/\/\/is\/a    |
-- +---------+---------------------------+---------------------------+----------------------+
-- | UNIX    | \/this\/is\/a\/test       | \/this\/is\/a\/           | \/this\/is\/a        |
-- +---------+---------------------------+---------------------------+----------------------+
-- | UNIX    | \/this\/is\/a\/test       | \/this\/is\/a             | \/this\/is\/a        |
-- +---------+---------------------------+---------------------------+----------------------+
-- | UNIX    | \/this\/is\/a\/test       | \/this\/is\/a\/string     | \/this\/is\/a        |
-- +---------+---------------------------+---------------------------+----------------------+
-- | WINDOWS | C:\/abc\/test.txt         | C:\/                      | C:\/                 |
-- +---------+---------------------------+---------------------------+----------------------+
-- | WINDOWS | C:\/abc\/test.txt         | C:\/def\/test.txt         | C:\/                 |
-- +---------+---------------------------+---------------------------+----------------------+
-- | WINDOWS | C:\/test\/abc.txt         | D:\/test\/abc.txt         | ""                   |
-- +---------+---------------------------+---------------------------+----------------------+
--
intersection :: CBytes      -- ^ The base path which will be compared with the other path.
             -> CBytes      -- ^ The other path which will compared with the base path.
             -> IO CBytes
intersection p1 p2 = do
    len <- withCBytesUnsafe p1 $ \ pp1 ->
        withCBytesUnsafe p2 $ \ pp2 ->
            cwk_path_get_intersection pp1 pp2
    if len == 0
    then return CB.empty
    else return (CB (V.PrimVector (CB.rawPrimArray p1) 0 (fromIntegral len)))

-- | Generates an absolute path based on a base.
--
-- This function generates an absolute path based on a base path and another path.
-- It is guaranteed to return an absolute path.
-- If the second submitted path is absolute, it will override the base path.
--
-- +----------------------+----------------------+-----------------------+
-- | Base                 | Path                 | Result                |
-- +----------------------+----------------------+-----------------------+
-- | \/hello\/there       | ..\/..\/..\/..\/..\/ | \/                    |
-- +----------------------+----------------------+-----------------------+
-- | \/hello\/\/..\/there | test\/\/thing        | \/there\/test\/thing  |
-- +----------------------+----------------------+-----------------------+
-- | hello\/there         | \/test               | \/test                |
-- +----------------------+----------------------+-----------------------+
-- | hello\/there         | test                 | \/hello\/there\/test  |
-- +----------------------+----------------------+-----------------------+
-- | \/hello\/there       | \/test               | \/test                |
-- +----------------------+----------------------+-----------------------+
-- | \/hello\/there       | ..                   | \/hello               |
-- +----------------------+----------------------+-----------------------+
--
absolute :: CBytes  -- ^ The absolute base path on which the relative path will be applied.
         -> CBytes  -- ^ The relative path which will be applied on the base path.
         -> IO CBytes
absolute p p2 = do
    let l = CB.length p + CB.length p2 + (#const BUF_EXT_SIZ)
    (p', _) <- withCBytesUnsafe p $ \ pp ->
        withCBytesUnsafe p2 $ \ pp2 ->
            allocCBytesUnsafe l $ \ pbuf ->
                cwk_path_get_absolute pp pp2 pbuf (fromIntegral l)
    return p'

-- | Generates a relative path based on a base.
--
-- This function generates a relative path based on a base path and another path.
-- It determines how to get to the submitted path, starting from the base directory.
--
-- +---------+--------------------------+--------------------------+-----------------+
-- | Style   | Base                     | Path                     | Result          |
-- +---------+--------------------------+--------------------------+-----------------+
-- | UNIX    | \/..\/..\/               | \/..\/..\/               | .               |
-- +---------+--------------------------+--------------------------+-----------------+
-- | UNIX    | \/path\/same             | \/path\/not_same\/ho\/.. | ..\/not_same    |
-- +---------+--------------------------+--------------------------+-----------------+
-- | UNIX    | \/path\/not_same\/ho\/.. | \/path\/same             | ..\/same        |
-- +---------+--------------------------+--------------------------+-----------------+
-- | UNIX    | \/path\/same             | \/path\/same\/ho\/..     | .               |
-- +---------+--------------------------+--------------------------+-----------------+
-- | UNIX    | \/path\/same\/ho\/..     | \/path\/same             | .               |
-- +---------+--------------------------+--------------------------+-----------------+
-- | UNIX    | \/path\/same             | \/path\/same             | .               |
-- +---------+--------------------------+--------------------------+-----------------+
-- | UNIX    | \/path\/long\/one        | \/path\/long\/one\/two   | two             |
-- +---------+--------------------------+--------------------------+-----------------+
-- | UNIX    | \/path\/long\/one\/two   | \/path\/long\/one        | ..              |
-- +---------+--------------------------+--------------------------+-----------------+
-- | UNIX    | .\/this\/is\/path_one    | .\/this\/is\/path_two    | ..\/path_two    |
-- +---------+--------------------------+--------------------------+-----------------+
-- | UNIX    | \/this\/is\/path_one     | \/this\/is\/path_two     | ..\/path_two    |
-- +---------+--------------------------+--------------------------+-----------------+
-- | WINDOWS | C:\/path\/same           | D:\/path\/same           | ""              |
-- +---------+--------------------------+--------------------------+-----------------+
--
relative :: CBytes  -- ^ The base path from which the relative path will start.
         -> CBytes  -- ^ The target path where the relative path will point to.
         -> IO CBytes
relative p p2 = do
    let l = CB.length p + CB.length p2 + (#const BUF_EXT_SIZ)
    (p', _) <- withCBytesUnsafe p $ \ pp ->
        withCBytesUnsafe p2 $ \ pp2 ->
            allocCBytesUnsafe l $ \ pbuf ->
                cwk_path_get_relative pp pp2 pbuf (fromIntegral l)
    return p'

-- | Split the extension of a file path.
--
-- This function extracts the extension portion of a file path.
--
-- +----------------------------+------------+
-- | Path                       | Result     |
-- +----------------------------+------------+
-- | \/my\/path.txt             | .txt       |
-- +----------------------------+------------+
-- | \/my\/path                 | ""         |
-- +----------------------------+------------+
-- | \/my\/.path                | .path      |
-- +----------------------------+------------+
-- | \/my\/path.                | .          |
-- +----------------------------+------------+
-- | \/my\/path.abc.txt.tests   | .tests     |
-- +----------------------------+------------+
--
splitExtension :: CBytes
               -> IO (CBytes, CBytes) -- ^ return (file, ext)
{-# INLINABLE splitExtension #-}
splitExtension p = do
    (len ,off) <- withCBytesUnsafe p $ \ pp ->
        allocPrimUnsafe $ \ plen ->
            hs_cwk_path_get_extension pp plen
    if off == -1
    then return (p, CB.empty)
    else return ( CB (V.PrimVector (CB.rawPrimArray p) 0 off)
                , CB (V.PrimVector (CB.rawPrimArray p) off len))

-- | Changes the extension of a file path.
--
-- This function changes the extension of a file name.
-- The function will append an extension if the basename does not have an extension,
-- or use the extension as a basename if the path does not have a basename.
--
-- Note:
--
-- * This function does not normalize the resulting path. You can use 'normalize' to do so.
-- * If the new_extension parameter starts with a dot,
--   the first dot will be ignored when the new extension is appended.
--
-- @
-- > changeExtension  "foo\/bar.txt" "png"
-- "foo\/bar.png"
-- @
--
changeExtension :: CBytes  -- ^ The path which will be used to make the change.
                -> CBytes  -- ^ The extension which will be placed within the new path.
                -> IO CBytes
changeExtension p p2 = do
    let l = CB.length p + CB.length p2 + (#const BUF_EXT_SIZ)
    (p', _) <- withCBytesUnsafe p $ \ pp ->
        withCBytesUnsafe p2 $ \ pp2 ->
            allocCBytesUnsafe l $ \ pbuf ->
                cwk_path_change_extension pp pp2 pbuf (fromIntegral l)
    return p'

--------------------------------------------------------------------------------

-- | Get a list of paths in the @$PATH@ variable.
getSearchPath :: IO [CBytes]
getSearchPath = do
    s <- getEnv' "PATH"
    sp <- searchPathSeparator
    return (splitSearchPath s sp)
  where
    splitSearchPath (CB bs) sp = go bs sp
    go bs sp = case V.break (== sp) bs of
        (p, rest)
            | V.null rest -> []
            | otherwise -> g p : go (V.drop 1 rest) sp
    g bs = if V.null bs then CB.singleton (#const DOT) else CB bs

--------------------------------------------------------------------------------

foreign import ccall unsafe hs_cwk_path_get_basename :: BA## Word8 -> MBA## Int -> IO Int
foreign import ccall unsafe cwk_path_change_basename :: BA## Word8 -> BA## Word8 -> MBA## Word8 -> CSize -> IO CSize
-- foreign import ccall unsafe hs_cwk_path_get_dirname :: BA## Word8 -> IO Int
foreign import ccall unsafe hs_cwk_path_get_root :: BA## Word8 -> IO Int
foreign import ccall unsafe cwk_path_change_root :: BA## Word8 -> BA## Word8 -> MBA## Word8 -> CSize -> IO CSize
foreign import ccall unsafe cwk_path_is_absolute :: BA## Word8 -> IO CBool
foreign import ccall unsafe cwk_path_is_relative :: BA## Word8 -> IO CBool
foreign import ccall unsafe cwk_path_join :: BA## Word8 -> BA## Word8 -> MBA## Word8 -> CSize -> IO CSize
foreign import ccall unsafe hs_cwk_path_join_multiple :: BAArray## Word8 -> Int -> MBA## Word8 -> CSize -> IO CSize
foreign import ccall unsafe cwk_path_normalize :: BA## Word8 -> MBA## Word8 -> CSize -> IO CSize
foreign import ccall unsafe cwk_path_get_intersection :: BA## Word8 -> BA## Word8 -> IO CSize
foreign import ccall unsafe cwk_path_get_absolute :: BA## Word8 -> BA## Word8 -> MBA## Word8 -> CSize -> IO CSize
foreign import ccall unsafe cwk_path_get_relative :: BA## Word8 -> BA## Word8 -> MBA## Word8 -> CSize -> IO CSize
foreign import ccall unsafe hs_cwk_path_get_extension :: BA## Word8 -> MBA## CSize -> IO Int
foreign import ccall unsafe cwk_path_change_extension :: BA## Word8 -> BA## Word8 -> MBA## Word8 -> CSize -> IO CSize
foreign import ccall unsafe cwk_path_guess_style :: BA## Word8 -> IO CInt
foreign import ccall unsafe cwk_path_get_style :: IO CInt
foreign import ccall unsafe cwk_path_set_style :: CInt -> IO ()
