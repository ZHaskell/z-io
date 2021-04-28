module Z.IO.StdStream.ReadLine where

import Control.Applicative
import qualified Z.Data.Text            as T
import qualified Z.Data.Text.Base       as T
import qualified Z.Data.Vector          as V
import qualified Z.Data.Parser          as P
import Data.Bits                        ((.|.), (.&.))
import Z.Data.ASCII
import Z.IO
import Z.IO.LowResTimer
import Prelude                          hiding (Left, Right)


data ReadLineConf =  ReadLineConf

defaultReadLineConf :: ReadLineConf
defaultReadLineConf = ReadLineConf

readLine :: HasCallStack
         => T.Text           -- ^ prompt
         -> IO Key
readLine prompt = withRawStdin . withMVar stdinBuf $ \ i -> do
    -- first we have to get key event
    readKey i
    -- then we have to edit the line buffer and refresh the line

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
