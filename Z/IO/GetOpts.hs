{-|
Module      : Z.IO.Process
Description : Process utilities
Copyright   : (c) Dong Han, 2018-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

This module provides command line options parsing utilities based on GHC generics. Supported syntax is similar to 
<https://man7.org/linux/man-pages/man3/getopt.3.html getopt>.

@
import Z.IO.GetOpts
import Z.Data.CBytes
...
 
data ServerOpts = ServerOpts 
    { port          :: CmdOpt "p" "The server's listening port"    "8888"  Int
    , configFile    :: CmdOpt "c" "The server's config file path"  ""      CBytes 
    , verbose       :: CmdFlg "v" "Run the server in verbose mode" 
    } deriving (Generic, CmdOpts)

main :: IO ()
main = do
    ServerOpts{..} <- getCmdOpts defaultCmdConfig
    conf <- quickReadFile (getCmdOpt configFile)
    startServer conf (getCmdOpt port) $ do
        if getCmdFlg verbose then ... else ...
@

Sub-command is supported by defining a sum type, with a 'SubCmd' field in each branch:

@
data MyGit a
    = MyGitClone { clone :: SubCmd "Clone a repository into a new directory"
                 , ... }
    | MyGitInit { init :: SubCmd "Create an empty Git repository or reinitialize an existing one"
                , ... } 
    ...
@

-}
module Z.IO.GetOpts where               
                                        
import Z.Data.Vector.FlatMap as FM
import Z.Data.Vector.FlatSet as FS
                                        

data CmdOpt a :: 

instance Monad CmdOpt where

instance Functor CmdOpt where
    fmap f = 


getCmdNum :: Char -> T.CBytes -> T.CBytes -> Maybe Scientific -> CmdOpt Scientific


getCmdOpts :: CmdOpt a -> IO a
getCmdOpts = do 
    processArgs =<< getArgs

data RawCmdOpt 
    = RawCmdFlagShort Char
    | RawCmdFlagLong CBytes
    | RawCmdOptShort (Char, [CBytes])
    | RawCmdOptFlagLong (CBytes, [CBytes])
    | RawArguments [CBytes]

processArgs :: [T.CBytes] -> [RawCmdOpt]
processArgs [] = []
processArgs (arg:[]) 
processArgs (arg:next:rest) 
    | isOpt next = 
    | otherwise 
    

