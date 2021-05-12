module Z.IO.StdStream.ReadLine
  ( -- * ReadStdLine prompt
    readStdLine
  , ReadLineConf(..), defaultReadLineConf
  , setGlobalPrompt
  , setReadLineConfig

  ) where

import Control.Applicative
import Control.Monad
import qualified Z.Data.Builder         as B
import qualified Z.Data.Text            as T
import qualified Z.Data.Text.Base       as T
import qualified Z.Data.Vector          as V
import qualified Z.Data.Parser          as P
import Z.Data.CBytes          (CBytes)
import qualified Z.IO.StdStream.Ansi    as Ansi
import Data.Bits                        ((.|.), (.&.))
import Data.IORef
import Z.Data.ASCII
import Z.IO
import Z.IO.LowResTimer
import Prelude                          hiding (Left, Right)
import System.IO.Unsafe                 (unsafePerformIO)

-------------------------------------------------------------------
-- Global state

data CharList
    -- | payload is char and its display width
    = CLCons {-# UNPACK #-} !Char {-# UNPACK #-} !Int CharList
    | CLNil

data CharZipper = CharZipper
    { leftPart :: !CharList
    , rightPart :: !CharList
    , leftWidth :: {-# UNPACK #-} !Int
    , allWidth :: {-# UNPACK #-} !Int
    }

emptyCZ :: CharZipper
emptyCZ = CharZipper CLNil CLNil 0 0

insertChunkCZ :: T.Text -> CharZipper -> CharZipper
insertChunkCZ t (CharZipper cl0 cr0 lw0 aw0) =
    let (cl, w) = go (T.unpack t) cl0 0 in CharZipper cl cr0 (lw0+w) (aw0+w)
  where
    go (c:cs) cl !w = let cw = T.displayWidthChar c in go cs (CLCons c cw cl) (w+cw)
    go _ cl w = (cl, w)

insertCZ :: Char -> CharZipper -> CharZipper
insertCZ c (CharZipper left right lw aw) =
    CharZipper (CLCons c cw left) right (lw + cw) (aw + cw)
  where cw = T.displayWidthChar c

backSpaceCZ :: CharZipper -> CharZipper
backSpaceCZ cz@(CharZipper left right lw aw) =
    case left of
        CLCons _ cw xs -> CharZipper xs right (lw - cw) (aw - cw)
        _ -> cz

backSpacesCZ :: Int -> CharZipper -> CharZipper
backSpacesCZ n cz | n > 0     = backSpacesCZ (n-1) (backSpaceCZ cz)
                  | otherwise = cz

deleteCZ :: CharZipper -> CharZipper
deleteCZ cz@(CharZipper cl cr lw aw) =
    case cr of
        CLCons _ cw xs -> CharZipper cl xs lw (aw - cw)
        _ -> cz

clearLeftCZ :: CharZipper -> CharZipper
clearLeftCZ (CharZipper _ cr lw aw) = CharZipper CLNil cr 0 (aw - lw)

clearRightCZ :: CharZipper -> CharZipper
clearRightCZ (CharZipper cl _ lw _) = CharZipper cl CLNil lw lw

moveLeftCZ :: CharZipper -> CharZipper
moveLeftCZ cz@(CharZipper cl0 cr0 lw aw) = go cl0 cr0
  where
    go (CLCons x cw xs) cr = CharZipper xs (CLCons x cw cr) (lw - cw) aw
    go _ _ = cz

moveRightCZ :: CharZipper -> CharZipper
moveRightCZ cz@(CharZipper cl0 cr0 lw aw) = go cl0 cr0
  where
    go cl (CLCons x cw xs) = CharZipper (CLCons x cw cl) xs (lw + cw) aw
    go _ _ = cz

moveToStartCZ :: CharZipper -> CharZipper
moveToStartCZ (CharZipper cl0 cr0 _ aw) = go cl0 cr0
  where
    go (CLCons x cw xs) cr = go xs (CLCons x cw cr)
    go _ cr = CharZipper CLNil cr 0 aw

moveToEndCZ :: CharZipper -> CharZipper
moveToEndCZ (CharZipper cl0 cr0 _ aw) = go cl0 cr0
  where
    go cl (CLCons x cw xs) = go (CLCons x cw cl) xs
    go cl _ = CharZipper cl CLNil aw aw

packCZLeft :: CharZipper -> T.Text
packCZLeft (CharZipper l _ _ _) = B.unsafeBuildText (goL l)
  where
    goL (CLCons c _ cs) = goL cs >> B.charUTF8 c
    goL _ = return ()

packCZRight :: CharZipper -> T.Text
packCZRight (CharZipper _ r _ _) = B.unsafeBuildText (goR r)
  where
    goR (CLCons c _ cs) = B.charUTF8 c >> goR cs
    goR _ = return ()

packCZ :: CharZipper -> T.Text
packCZ (CharZipper l r _ _) = B.unsafeBuildText (goL l >> goR r)
  where
    goL (CLCons c _ cs) = goL cs >> B.charUTF8 c
    goL _ = return ()
    goR (CLCons c _ cs) = B.charUTF8 c >> goR cs
    goR _ = return ()


data ReadLineState = ReadLineState
    { lineBuffer :: CharZipper
    , history :: Zipper T.Text
    , globalPrompt :: T.Text
    , completionPrefix :: T.Text
    , completion :: Zipper (T.Text, T.Text)
    , readLineConf :: ReadLineConf
    }

type Zipper a = ([a], Maybe a, [a])

resetZipperToLeft :: Zipper a -> [a]
resetZipperToLeft (hl0, mh, hr0) =
    case mh of Just h' -> loop (h':hl0) hr0
               _ -> loop hl0 hr0
  where
    loop hl (h:hr) = loop (h:hl) hr
    loop hl _ = hl

nullZipper :: Zipper a -> Bool
nullZipper (cl, mc, cr) = null cl && null mc && null cr

emptyZipper :: Zipper a
emptyZipper = ([], Nothing, [])

stepZipper :: Zipper a ->  Zipper a
stepZipper (cl, mc, c:cr') =
    case mc of Just c' -> (c':cl, Just c, cr')
               _ ->  (cl, Just c, cr')
stepZipper (cl, mc, _) =
    case mc of Just c' -> (c':cl, Nothing, [])
               _ -> stepZipper ([], Nothing, reverse cl)

-- | TODO: compute row using column
buildCompletion :: T.Text
                -> Zipper (T.Text, T.Text)
                -> B.Builder Int
buildCompletion prefix (cl, mc, cr) = do
    x <- goL cl 0
    y <- case mc of
        Just c -> Ansi.invert >> printComp c >> Ansi.invertOff >> return 1
        _ -> return 0
    z <- goR cr 0
    return $! x + y + z
  where
    goL (c:cs) !acc = goL cs (acc+1) <* printComp c
    goL _ !acc = return acc
    goR (c:cs) !acc = printComp c >> goR cs (acc+1)
    goR _ !acc = return acc
    printComp (comp, comment) = do
        "Wtf ?"
        B.text prefix
        B.text comp
        Ansi.cursorForward (max (32 - T.displayWidth comp) 4)
        B.text comment
        B.word8 NEWLINE

data ReadLineConf = ReadLineConf
    { historyFile :: CBytes
    , maxHistory :: Int
    , onComplete :: T.Text  -- ^ left to cursor
                 -> T.Text  -- ^ right to cursor
                 -> IO (T.Text, [(T.Text, T.Text)]) -- ^ return (prefix, [(candidate, comment)])
    , onETX :: IO ()
    }

newReadLineState :: ReadLineConf -> IO ReadLineState
newReadLineState conf = do
    return ReadLineState
        { lineBuffer = emptyCZ
        , history = emptyZipper
        , globalPrompt = T.empty
        , completionPrefix = T.empty
        , completion = emptyZipper
        , readLineConf = conf
        }

defaultReadLineConf :: ReadLineConf
defaultReadLineConf = ReadLineConf
    { historyFile = ""
    , maxHistory = -1
    , onComplete = \ _ _ -> return (T.empty, [])
    , onETX = putStd $ "Ctrl+C pressed\n"
    }

readLineState :: IORef ReadLineState
{-# NOINLINE readLineState #-}
readLineState = unsafePerformIO $
    newIORef =<< newReadLineState defaultReadLineConf

getReadLineState :: IO ReadLineState
getReadLineState = readIORef readLineState

setReadLineState :: ReadLineState -> IO ()
setReadLineState !s = writeIORef readLineState s

setReadLineConfig :: ReadLineConf -> IO ()
setReadLineConfig conf = writeIORef readLineState =<< newReadLineState conf

--------------------------------------------------------------------------------

setGlobalPrompt :: T.Text -> IO ()
setGlobalPrompt p = do
    s <- getReadLineState
    setReadLineState s{globalPrompt = T.filter (>= ' ') p}

readStdLine :: HasCallStack
         => T.Text          -- ^ prompt
         -> IO T.Text
readStdLine p =
    withMVar stdinBuf $ \ stdin_ -> withRawStdin $ do
        s <- getReadLineState
        let prompt = globalPrompt s <> T.filter (>= ' ') p
            promptWidth = T.displayWidth prompt
        putStd $ B.text prompt
        handleKeyLoop promptWidth stdin_

handleKeyLoop :: Int -> BufferedInput -> IO T.Text
handleKeyLoop promptWidth stdin_ = loop
  where
    loop = do
        -- first we have to get key event
        key <- readKey stdin_
        -- then we have to edit the line buffer and refresh the line
        -- reference: http://jkorpela.fi/chars/c0.html
        case key of
            Key _ Up            -> prevHistory >> loop
            Key _ Down          -> nextHistory >> loop
            Key _ Backspace     -> backSpace >> loop
            Key _ Delete        -> delete >> loop
            Key _ Left          -> moveLeft >> loop
            Key _ Right         -> moveRight >> loop
            Key _ Home          -> moveToStart >> loop
            Key _ End           -> moveToEnd >> loop
            Key _ Esc           -> triggerBEL >> loop

            Key _ (Char '\r')   -> finishLine
            -- ctrl u
            Key _ (Char '\NAK') -> clearLeft >> loop

            -- tab, trigger completion
            Key _ (Char '\t')   -> onComp >> loop
            -- ctrl a
            Key _ (Char '\SOH') -> moveToStart >> loop
            -- ctrl b
            Key _ (Char '\STX') -> moveLeft >> loop
            -- ctrl c
            Key _ (Char '\ETX') -> finishLine >> triggerETX >> return T.empty
            -- ctrl d
            Key _ (Char '\EOT') -> delete >> loop
            -- ctrl e
            Key _ (Char '\ENQ') -> moveToEnd >> loop
            -- ctrl f
            Key _ (Char '\ACK') -> moveRight >> loop
            -- ctrl g
            Key _ (Char '\BEL') -> triggerBEL >> loop
            -- ctrl h
            Key _ (Char '\BS')  -> backSpace >> loop
            Key _ (Char c)      -> addChar c >> loop

            -- fn, pagedown, pageup
            Key _ _             -> loop

    writeLeft !col end (CLCons c cw cs) | col < end = do
        writeLeft (col+cw) end cs
        B.charUTF8 c
    writeLeft _ _ _ = return ()

    writeRight (CLCons c _ cs) = do
        B.charUTF8 c
        writeRight cs
    writeRight _ = return ()

    resetCursor w w' ttyWidth = do
        let (row, col) =  w `quotRem`  ttyWidth
            (row', col') =  w' `quotRem`  ttyWidth
        Ansi.cursorUp (row - row')
        Ansi.cursorDown (row' - row)
        Ansi.cursorBackward (col - col')
        Ansi.cursorForward (col' - col)

    loadLineBuffer cz = do
        s <- getReadLineState
        let s' = s{ lineBuffer = cz }
        setReadLineState s'

        (ttyWidth, _) <- getStdoutWinSize
        let lw  = promptWidth + leftWidth (lineBuffer s)
            lw' = promptWidth + leftWidth (lineBuffer s')
            aw' = promptWidth + allWidth (lineBuffer s')

        putStd $ do
            resetCursor lw promptWidth ttyWidth            -- clear old
            writeLeft 0 lw' (leftPart $ lineBuffer s')
            writeRight (rightPart $ lineBuffer s')
            when (aw' `rem`  ttyWidth == 0) $ (B.word8 NEWLINE)
            Ansi.clearFromCursorToScreenEnd
            resetCursor aw' lw' ttyWidth

    modifyLineBuffer f = do
        s <- getReadLineState
        let s' = s{ lineBuffer = f (lineBuffer s) , completion = emptyZipper }
        setReadLineState s'

        (ttyWidth, _) <- getStdoutWinSize
        let lw  = promptWidth + leftWidth (lineBuffer s)
            lw' = promptWidth + leftWidth (lineBuffer s')
            aw' = promptWidth + allWidth (lineBuffer s')

        putStd $ do
            if lw < lw'
            then writeLeft lw lw' (leftPart $ lineBuffer s')    -- insert
            else resetCursor lw lw' ttyWidth                    -- delete
            writeRight (rightPart $ lineBuffer s')
            when (lw <= aw' && aw' `rem`  ttyWidth == 0) $ (B.word8 NEWLINE)
            Ansi.clearFromCursorToScreenEnd
            resetCursor aw' lw' ttyWidth

        unless (nullZipper $ completion s) onComp


    -- for cursor move only
    moveLineBuffer f = do
        s <- getReadLineState
        let s' = s{ lineBuffer = f (lineBuffer s), completion = emptyZipper }
        setReadLineState s'
        (ttyWidth, _) <- getStdoutWinSize
        let lw  = promptWidth + leftWidth (lineBuffer s)
            lw' = promptWidth + leftWidth (lineBuffer s')
        putStd $ resetCursor lw lw' ttyWidth

        unless (nullZipper $ completion s) onComp


    finishLine = do
        moveLineBuffer moveToEndCZ
        s <- getReadLineState
        let !t = packCZ (lineBuffer s)
            !hl = resetZipperToLeft (history s)
        if T.null t
        then setReadLineState s{ lineBuffer = emptyCZ }
        else setReadLineState s{ lineBuffer = emptyCZ
                               , history = (t:hl, Nothing, []) }
        putStd $ B.word8 NEWLINE >> Ansi.setCursorColumn 1
        return t

    addChar c = modifyLineBuffer (insertCZ c)
    backSpace = modifyLineBuffer backSpaceCZ
    delete = modifyLineBuffer deleteCZ
    clearLeft = modifyLineBuffer clearLeftCZ

    moveLeft    = moveLineBuffer moveLeftCZ
    moveRight   = moveLineBuffer moveRightCZ
    moveToStart = moveLineBuffer moveToStartCZ
    moveToEnd   = moveLineBuffer moveToEndCZ

    onComp = do
        s <- getReadLineState
        let comp@(_,mc,_) = completion s
        if nullZipper comp
        then do
            let !tl = packCZLeft (lineBuffer s)
                !tr = packCZRight (lineBuffer s)
            (prefix, candidates) <- (onComplete (readLineConf s)) tl tr
            if null candidates
            then triggerBEL
            else do
                setReadLineState s{ completionPrefix = prefix }
                refreshCompletion ([], Nothing, candidates)
        else do
            let comp'@(_,mc',_) = stepZipper comp
            forM_ mc $ \ (c, _) -> modifyLineBuffer (backSpacesCZ (T.length c))
            forM_ mc' $ \ (c', _) -> modifyLineBuffer (insertChunkCZ c')
            refreshCompletion comp'

    refreshCompletion comp = do
        s <- getReadLineState
        setReadLineState s{ completion = comp }

        (ttyWidth, _) <- getStdoutWinSize

        let lw  = promptWidth + leftWidth (lineBuffer s)
            aw  = promptWidth + allWidth (lineBuffer s)
            c_start_row = aw `quot` ttyWidth

        putStd $ do
            resetCursor lw aw ttyWidth
            B.word8 NEWLINE
            Ansi.clearFromCursorToScreenEnd
            c_row <- buildCompletion (completionPrefix s) comp
            Ansi.setCursorColumn 1
            Ansi.cursorUp (c_start_row + c_row + 1)
            resetCursor 0 lw ttyWidth

    prevHistory = do
        s <- getReadLineState
        let (hl, mh, hr) = history s
        case hl of
            (h:hl') -> do
                case mh of
                    Just h' -> setReadLineState s { history = (hl', Just h, h':hr) }
                    _ -> setReadLineState s { history = (hl', Just h, hr) }
                loadLineBuffer $ insertChunkCZ h emptyCZ
            _ -> case mh of
                Just h -> setReadLineState s { history = (hl, Nothing, h:hr) }
                _ -> triggerBEL


    nextHistory = do
        s <- getReadLineState
        let (hl, mh, hr) = history s
        case hr of
            (h:hr') -> do
                case mh of
                    Just h' -> setReadLineState s { history = (h':hl, Just h, hr') }
                    _ -> setReadLineState s { history = (hl, Just h, hr) }
                loadLineBuffer $ insertChunkCZ h emptyCZ
            _ -> case mh of
                Just h -> setReadLineState s { history = (h:hl, Nothing, hr) }
                _ -> triggerBEL

    triggerETX = onETX . readLineConf =<< getReadLineState
    triggerBEL = putStd $ B.word8 7

-- | Get a single key event from tty.
--
readKey :: HasCallStack => BufferedInput -> IO Key
readKey i = do
    bs <- readBuffer i
    (rest, r) <- P.parseChunks (P.parseChunk keyParser) timeoutRead bs
    unReadBuffer rest i
    unwrap "EPARSE" r
  where
    timeoutRead = do
        -- 200ms timeout
        bs <- timeoutLowRes 2 (readBuffer i)
        case bs of Just bs' -> return bs'
                   _  -> return V.empty

--------------------------------------------------------------------------------

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
