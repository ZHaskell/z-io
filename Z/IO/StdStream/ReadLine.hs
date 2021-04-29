module Z.IO.StdStream.ReadLine where

import Control.Applicative
    ( Applicative(pure), Alternative((<|>)) )
import qualified Z.Data.Text            as T
import qualified Z.Data.Text.Base       as T
import qualified Z.Data.Vector          as V
import qualified Z.Data.Parser          as P
import Z.Data.Builder ( Builder, stringUTF8 )
import Data.Bits                        ((.|.), (.&.))
import Z.Data.ASCII
import Z.IO
    ( withMVar,
      HasCallStack,
      unwrap,
      readBuffer,
      unReadBuffer,
      BufferedInput,
      stdinBuf,
      withRawStdin,
      putStd  )
import Z.IO.LowResTimer ( timeoutLowRes )
import Prelude                          hiding (Left, Right)
import Control.Monad.State ( StateT(..) )
import Control.Monad ( unless )
import qualified Z.IO.StdStream.Ansi as Ansi
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import System.IO.Unsafe ( unsafePerformIO )
import Data.List ( isPrefixOf )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

-------------------------------------------------------------------
--Global state
{-# NOINLINE globalKeywords #-}
globalKeywords :: IORef [String]
globalKeywords = unsafePerformIO (newIORef [])

setKeyWords :: [String]->IO()
setKeyWords = writeIORef globalKeywords

getKeyWords :: IO [String]
getKeyWords = readIORef globalKeywords
--------------------------------------------------------------------

readKeyfromInput :: HasCallStack => IO Key
readKeyfromInput = withRawStdin . withMVar stdinBuf $ \i -> readKey i

type InputControl a = StateT CommandLine IO a

data CommandLine = CommandLine {
    line :: (String,String), -- | S x S is zipper
    history :: ([String], [String])
}

instance Show CommandLine where
    show (CommandLine (cursorL, cursorR) _) = reverse cursorL ++ cursorR

newCommandLine :: IO CommandLine
newCommandLine = return (CommandLine ("","") ([], []))

addHistory' :: CommandLine->CommandLine
addHistory' com@(CommandLine ("", "") _) = com
addHistory' com@(CommandLine (l, r) (hist, zipper))
    = com{history=( (reverse l ++ r):(hist ++ reverse zipper) ,[])}

lastHistory' :: CommandLine->CommandLine
lastHistory' commandline@(CommandLine _ ([], _)) = commandline
lastHistory' commandline@(CommandLine _ (his@[item], zipper)) 
    = commandline{
        history=(his, zipper),
        line=(reverse item, "")
      }
lastHistory' commandline@(CommandLine _ (h:hs, zipper)) =
    commandline{
        history=(hs, h:zipper),
        line=(reverse h, "")
    }

nextHistory' :: CommandLine->CommandLine
nextHistory' commandline@(CommandLine _ (_, [])) = commandline
nextHistory' commandline@(CommandLine _ (his, zipper@[item]))
    = commandline{
        history=(his, zipper),
        line=(reverse item, "")
    }
nextHistory' commandline@(CommandLine (_, _) (hs, z:zipper)) =
    commandline{
        history=(z:hs, zipper),
        line = (reverse z, "")
    }

addChar' :: Char->CommandLine->CommandLine
addChar' c commandline@(CommandLine (l, r) _) = commandline{line=(c:l, r)}

clearLine' :: CommandLine->CommandLine
clearLine' commandline= commandline{line=("", "")}

putLine :: InputControl ()
putLine = do
    com <- getCommandLine
    let built = stringUTF8 $ show com
    let prompt = stringUTF8 "> "
    let new_line = prompt <> built
    printCommand $ do 
        Ansi.clearLine
        Ansi.setCursorColumn 0 
        new_line
    
getCommandLine :: InputControl CommandLine
getCommandLine = StateT $ \com->return (com, com)

modifyCommandLine :: CommandLine->InputControl ()
modifyCommandLine new_com = StateT $ \_->return ((), new_com)

printCommand :: Builder ()->InputControl ()
printCommand !content = StateT $ \com->do
    putStd content
    return ((), com)

addChar :: Char->InputControl ()
addChar c = do
    com <- getCommandLine 
    (modifyCommandLine . addChar' c) com
    com_changed <- getCommandLine
    let built = stringUTF8 $ show com_changed
    let prompt = stringUTF8 "> "
    let new_line = prompt <> built
    (cur_col, cur_row) <- getCursorPos
    let start_position = cur_col + 1
    printCommand $ do
        Ansi.setCursorPosition 0 cur_row
        Ansi.clearLine
        new_line
        Ansi.setCursorPosition start_position cur_row

readKeyState :: InputControl Key
readKeyState = StateT $ \commandline->do
    key <- readKeyfromBuffer
    return (key, commandline)

addHistory :: InputControl ()
addHistory = getCommandLine >>= (modifyCommandLine . addHistory')

moveCursorR' :: CommandLine->CommandLine
moveCursorR' commandline@(CommandLine (_, []) _) = commandline
moveCursorR' commandline@(CommandLine (l, x:r) _) = do
     commandline{line=(x:l, r)}

moveCursorL' :: CommandLine->CommandLine
moveCursorL' commandline@(CommandLine ([], _) _) = commandline
moveCursorL' commandline@(CommandLine (x:l, r) _) = do
     commandline{line=(l, x:r)}

moveCursorR :: InputControl ()
moveCursorR = do 
    com <- getCommandLine
    --printCommand $ stringUTF8 (show $ line com)
    case com of
        CommandLine (_, []) _-> return ()
        commandline@(CommandLine (l, x:r) _)->do
            modifyCommandLine commandline{line=(x:l, r)}
            printCommand $ Ansi.cursorForward 1

moveCursorL :: InputControl ()
moveCursorL = do 
    com <- getCommandLine
    --printCommand $ stringUTF8 (show $ line com)
    case com of
        CommandLine ([], _) _-> return ()
        commandline@(CommandLine (x:l, r) _)->do
            modifyCommandLine commandline{line=(l, x:r)}
            printCommand $ Ansi.cursorBackward 1

getCursorPos :: InputControl (Int, Int)
getCursorPos = StateT $ \com->do
    (cur_col, cur_row) <- Ansi.getCursorPosition
    return ((cur_col, cur_row),com)

deleteChar' :: CommandLine->CommandLine
deleteChar' commandline@(CommandLine ([], _) _) = commandline
deleteChar' commandline@(CommandLine (_:ls, r) _) = commandline{line=(ls, r)}

deleteChar :: InputControl ()
deleteChar = do
    com <- getCommandLine 
    (modifyCommandLine . deleteChar') com
    com_changed <- getCommandLine
    let built = stringUTF8 $ show com_changed
    let prompt = stringUTF8 "> "
    let new_line = prompt <> built
    (cur_col, cur_row) <- getCursorPos
    let start_position = if cur_col <= 3 then cur_col else cur_col - 1
    printCommand $ do
        Ansi.setCursorPosition 0 cur_row
        Ansi.clearLine
        new_line
        Ansi.setCursorPosition start_position cur_row

lastHistory :: InputControl ()
lastHistory = getCommandLine >>= (modifyCommandLine . lastHistory')

nextHistory :: InputControl ()
nextHistory = getCommandLine >>= (modifyCommandLine . nextHistory')

clearLine :: InputControl ()
clearLine = getCommandLine >>= (modifyCommandLine . clearLine')

newLine :: InputControl ()
newLine = do 
    StateT $ \commandline->do
        putStd "\n> "
        return ((), commandline)
    clearLine

hintOrCompletion :: InputControl ()
hintOrCompletion = do
    com@(CommandLine (l, r) _) <- getCommandLine
    let (prefix, rest) = break (== ' ') l
    wordlist <- liftIO getKeyWords
    let reverse_prefix = reverse prefix
    let word_list = [x | x <- wordlist, reverse_prefix `isPrefixOf` x]
    printCommand $ stringUTF8 (show word_list)
    case word_list of
        [] -> return ()
        [single_word] ->modifyCommandLine com{
            line=(reverse single_word ++ rest,r)
         }
        _ ->printCommand $ stringUTF8 (show word_list)

execute :: InputControl ()
execute = undefined

readKeyfromBuffer :: HasCallStack => IO Key
readKeyfromBuffer = withRawStdin . withMVar stdinBuf $ \i -> readKey i

readLineState :: InputControl ()
readLineState = do
    key <- readKeyState
    terminate <- case key of
        Key (Modifier False False False) (Char '\r')->do
            addHistory
            putLine
            -- TODO: add execute
            newLine
            return False
        Key _ (Char '\NAK')->do 
            clearLine 
            putLine
            return False
        Key _ (Char '\t')->do 
            hintOrCompletion
            putLine 
            return False
        Key _ (Char '\ETX')->do 
            moveCursorR 
            putLine
            return True
        Key _ Up -> do
            lastHistory
            putLine
            return False
        Key _ Down -> do
            nextHistory
            putLine
            return False
        Key _ (Char c)->do
            addChar c
            --putLine
            return False
        Key _ Backspace->do 
            deleteChar
            return False
        Key _ Left->do 
            moveCursorL
            return False
        Key _ Right->do 
            moveCursorR
            return False
        Key _ _ ->return False
    unless terminate readLineState
--------------------------------------------------------------------------
readLine :: HasCallStack => IO ()
readLine = do
    command_line <- newCommandLine
    putStd $ stringUTF8 "> "
    _ <- runStateT readLineState command_line
    putStrLn "\nBye."

-- | Get a single key event from tty.
--
readKey :: HasCallStack => BufferedInput -> IO Key
readKey i = do
    bs <- readBuffer i
    -- debug
    --printStd bs
    (rest, r) <- P.parseChunks keyParser timeoutRead bs
    unReadBuffer rest i
    unwrap "EPARSE" r
  where
    timeoutRead = do
        -- 200ms timeout
        bs <- timeoutLowRes 2 (readBuffer i)
        case bs of Just bs' -> return bs'
                   _  -> return V.empty

data Key = Key Modifier BaseKey
    deriving (Eq, Ord, Show)

data Modifier = Modifier { ctrl, meta, shift :: !Bool }
    deriving (Eq, Ord, Show)

noModifier :: Modifier
noModifier = Modifier False False False

ctrlModifier :: Modifier
ctrlModifier = Modifier True False False

metaModifier :: Modifier
metaModifier = Modifier False True False

shiftModifier :: Modifier
shiftModifier = Modifier False False True

fromCModifier :: Int -> Modifier
fromCModifier x = Modifier (x .&. 4 /= 0) (x .&. 10 /= 0) (x .&. 1 /= 0)

data BaseKey = Char Char
             | Fn Int
             | Left | Right | Down | Up
             | Clear | Home | Insert | Delete | End | PageDown | PageUp
             | Backspace | Esc
            deriving (Show,Eq,Ord)

-- | This is a streamed key parser to deal with various escaped sequences.
--
-- Reference: https://github.com/nodejs/node/blob/master/lib/internal/readline/utils.js
--
-- Some patterns seen in terminal key escape codes, derived from combos seen
-- at http://www.midnight-commander.org/browser/lib/tty/key.c
-- ESC letter
-- ESC [ letter
-- ESC [ modifier letter
-- ESC [ 1 ; modifier letter
-- ESC [ num char
-- ESC [ num ; modifier char
-- ESC O letter
-- ESC O modifier letter
-- ESC O 1 ; modifier letter
-- ESC N letter
-- ESC [ [ num ; modifier char
-- ESC [ [ 1 ; modifier letter
-- ESC ESC [ num char
-- ESC ESC O letter
-- char is usually ~ but $ and ^ also happen with rxvt
-- modifier is 1 +
--           (shift     * 1) +
--           (left_alt  * 2) +
--           (ctrl      * 4) +
--           (right_alt * 8)
-- two leading ESCs apparently mean the same as one leading ESC
--
keyParser :: P.Parser Key
keyParser = do
    w <- P.anyCharUTF8
    case w of
        '\ESC' -> do
            end <- P.atEnd
            if end
            then return $ Key noModifier Esc
            else do
                w' <- P.anyCharUTF8
                end' <- P.atEnd
                if w' == '\ESC'
                then if end'
                    then return $ Key metaModifier Esc
                    else escapedKeyParser =<< P.anyCharUTF8
                else escapedKeyParser w'
            -- ctrl+letter
        _ | w == '\r' || w == '\n' || w == '\t' || w == ' ' ->
            return $ Key noModifier (Char w)
          | w == '\b' || w == '\DEL' -> return $ Key noModifier Backspace
          | w <= '\x1a'              -> return $ Key ctrlModifier (Char w)
          | otherwise                -> return $ Key noModifier (Char w)
  where
    --
    --  We have basically two classes of ascii characters to process:
    --
    --
    --  1. `\x1b[24;5~` should be parsed as { code: '[24~', modifier: 5 }
    --
    --  This particular example is featuring Ctrl+F12 in xterm.
    --
    --   - `;5` part is optional, e.g. it could be `\x1b[24~`
    --   - first part can contain one or two digits
    --
    --  So the generic regexp is like /^\d\d?(;\d)?[~^$]$/
    --
    --
    --  2. `\x1b[1;5H` should be parsed as { code: '[H', modifier: 5 }
    --
    --  This particular example is featuring Ctrl+Home in xterm.
    --
    --   - `1;5` part is optional, e.g. it could be `\x1b[H`
    --   - `1;` part is optional, e.g. it could be `\x1b[5H`
    --
    --  So the generic regexp is like /^((\d;)?\d)?[A-Za-z]$/
    --
    escapedKeyParser w
        | w == '[' || w == 'O' || w == 'N' = do
            (code, cmodifier) <- getEscapedSequence1
                <|> getEscapedSequence2
                <|> getEscapedSequence3
                <|> getEscapedSequence4
                <|> getEscapedSequence5

            let modifier = fromCModifier cmodifier
                mkKey = pure . Key modifier
                shiftKey = pure . Key shiftModifier
                ctrlKey = pure . Key ctrlModifier

            case w `T.cons` code of
                -- xterm/gnome ESC [ letter (with modifier)
                "[P" -> mkKey (Fn 1)
                "[Q" -> mkKey (Fn 2)
                "[R" -> mkKey (Fn 3)
                "[S" -> mkKey (Fn 4)

                -- xterm/gnome ESC O letter (without modifier)
                "OP" -> mkKey (Fn 1)
                "OQ" -> mkKey (Fn 2)
                "OR" -> mkKey (Fn 3)
                "OS" -> mkKey (Fn 4)

                -- xterm/rxvt ESC [ number ~
                "[11~" -> mkKey (Fn 1)
                "[12~" -> mkKey (Fn 2)
                "[13~" -> mkKey (Fn 3)
                "[14~" -> mkKey (Fn 4)

                -- from Cygwin and used in libuv
                "[[A" -> mkKey (Fn 1)
                "[[B" -> mkKey (Fn 2)
                "[[C" -> mkKey (Fn 3)
                "[[D" -> mkKey (Fn 4)
                "[[E" -> mkKey (Fn 5)

                -- common
                "[15~" -> mkKey (Fn 5)
                "[17~" -> mkKey (Fn 6)
                "[18~" -> mkKey (Fn 7)
                "[19~" -> mkKey (Fn 8)
                "[20~" -> mkKey (Fn 9)
                "[21~" -> mkKey (Fn 10)
                "[23~" -> mkKey (Fn 11)
                "[24~" -> mkKey (Fn 12)

                -- xterm ESC [ letter
                "[A" -> mkKey Up
                "[B" -> mkKey Down
                "[C" -> mkKey Right
                "[D" -> mkKey Left
                "[E" -> mkKey Clear
                "[F" -> mkKey End
                "[H" -> mkKey Home

                -- xterm/gnome ESC O letter
                "OA" -> mkKey Up
                "OB" -> mkKey Down
                "OC" -> mkKey Right
                "OD" -> mkKey Left
                "OE" -> mkKey Clear
                "OF" -> mkKey End
                "OH" -> mkKey Home

                -- xterm/rxvt ESC [ number ~
                "[1~" -> mkKey Home
                "[2~" -> mkKey Insert
                "[3~" -> mkKey Delete
                "[4~" -> mkKey End
                "[5~" -> mkKey PageUp
                "[6~" -> mkKey PageDown

                -- putty
                "[[5~" -> mkKey PageUp
                "[[6~" -> mkKey PageDown

                -- rxvt
                "[7~" -> mkKey Home
                "[8~" -> mkKey End

                -- rxvt keys with modifiers
                "[a" -> shiftKey Up
                "[b" -> shiftKey Down
                "[c" -> shiftKey Right
                "[d" -> shiftKey Left
                "[e" -> shiftKey Clear

                "[2$" -> shiftKey Insert
                "[3$" -> shiftKey Delete
                "[5$" -> shiftKey PageUp
                "[6$" -> shiftKey PageDown
                "[7$" -> shiftKey Home
                "[8$" -> shiftKey End

                "Oa" -> ctrlKey Up
                "Ob" -> ctrlKey Down
                "Oc" -> ctrlKey Right
                "Od" -> ctrlKey Left
                "Oe" -> ctrlKey Clear

                "[2^" -> ctrlKey Insert
                "[3^" -> ctrlKey Delete
                "[5^" -> ctrlKey PageUp
                "[6^" -> ctrlKey PageDown
                "[7^" -> ctrlKey Home
                "[8^" -> ctrlKey End

                -- misc.
                "[Z" -> shiftKey (Char '\t')
                _ -> P.fail' "can't parse escaped sequence"

        | w == '\r' || w == '\n' || w == '\t' || w == ' ' =
            return $ Key metaModifier (Char w)
        | w == '\b' || w == '\DEL' = return $ Key metaModifier Backspace
        | w == '\ESC'              = return $ Key metaModifier Esc
        | w <= '\x1a'              = return $ Key (Modifier True True False) (Char w)
        | otherwise                = return $ Key metaModifier (Char w)

    getEscapedSequence1 = do
        code <- P.takeWhile1 isDigit
        P.char8 ';'
        modifier <- P.int
        t <- P.satisfy $ \ t -> t == TILDE || t == DOLLAR || t == CIRCUM
        return (T.Text code `T.snoc` (w2c t), (modifier .|. 1) - 1)

    getEscapedSequence2 = do
        code <- P.takeWhile1 isDigit
        t <- P.satisfy $ \ t -> t == TILDE || t == DOLLAR || t == CIRCUM
        return (T.Text code `T.snoc` (w2c t), 0)

    getEscapedSequence3 = do
        P.char8 '1'
        P.char8 ';'
        modifier <- P.int
        code <- P.anyChar7
        return (T.singleton code, (modifier .|. 1) - 1)

    getEscapedSequence4 = do
        modifier <- P.int
        code <- P.anyChar7
        return (T.singleton code, (modifier .|. 1) - 1)

    getEscapedSequence5 = do
        code <- P.anyChar7
        return (T.singleton code, 0)
