{-|
Module      : Z.IO.StdStream.Ansi
Description : Ansi control code sequences
Copyright   : (c) Winterland, 2017-2020
License     : BSD
Maintainer  : winterland1989@gmail.com
Stability   : experimental
Portability : non-portable

Provides utilities to build <ANSI code sequences https://en.wikipedia.org/wiki/ANSI_escape_code>.

@
> putStd . bold . italicize . color Red  $ "hello"
hello   -- bold, italicize and red
@

-}

module Z.IO.StdStream.Ansi
  ( -- * Style modifier
    bold, italicize, underline,
    color, color', palette, palette', rgb, rgb',
    -- * Control codes
    cursorUp, cursorDown, cursorForward, cursorBackward,
    cursorDownLine, cursorUpLine ,
    setCursorColumn, setCursorPosition, saveCursor, restoreCursor, getCursorPosition,
    clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen,
    clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine,
    scrollPageUp, scrollPageDown,
    hideCursor, showCursor,
    setTitle,
    -- * Style codes
    reset,
    boldIntensity, faintIntensity, resetIntensity,
    italicized, noItalicized,
    singleUnderline, doubleUnderline, noUnderline,
    slowBlink, rapidBlink, blinkOff,
    conceal, reveal,
    invert, invertOff,
    setForeground, setBrightForeground, setBackground, setBrightBackground,
    setPaletteForeground, setPaletteBackground,
    setRGBForeground, setRGBBackground,
    setDefaultForeground, setDefaultBackground,
    AnsiColor(..), PaletteColor, RGBColor,
    -- * Internal helper
    csi, sgr, colorToCode
  ) where

import qualified Z.Data.Builder as B
import qualified Z.Data.Parser as P
import qualified Z.Data.Text    as T
import Z.Data.ASCII
import Data.Word
import GHC.Generics
import Z.IO.StdStream
import Z.IO.Buffered

csi :: [Int]  -- ^ List of parameters for the control sequence
    -> B.Builder () -- ^ Character(s) that identify the control function
    -> B.Builder ()
csi args code = do
    B.char8 '\ESC'
    B.char8 '['
    B.intercalateList (B.char8 ';') B.int args
    code

cursorUp, cursorDown, cursorForward, cursorBackward
  :: Int -- ^ Number of lines or characters to move
  -> B.Builder ()
cursorDownLine, cursorUpLine :: Int -- ^ Number of lines to move
                                     -> B.Builder ()
cursorUp n       = csi [n] (B.char8 'A')
cursorDown n     = csi [n] (B.char8 'B')
cursorForward n  = csi [n] (B.char8 'C')
cursorBackward n = csi [n] (B.char8 'D')
cursorDownLine n = csi [n] (B.char8 'E')
cursorUpLine n   = csi [n] (B.char8 'F')

getCursorPosition :: IO (Int, Int)
getCursorPosition = do
    withRawStdin . withMVar stdinBuf $ \ i -> do
        clearInputBuffer i
        putStd (csi [] "6n")
        readParser (do
            P.word8 ESC
            P.word8 BRACKET_LEFT
            !n <- P.int
            P.word8 SEMICOLON
            !m <- P.int
            P.word8 LETTER_R
            return (m, n)) i

-- | Code to move the cursor to the specified column. The column numbering is
-- 1-based (that is, the left-most column is numbered 1).
setCursorColumn :: Int -- ^ 1-based column to move to
                -> B.Builder ()
setCursorColumn n = csi [n] (B.char8 'G')

-- | Code to move the cursor to the specified position (row and column). The
-- position is 1-based (that is, the top-left corner is at row 1 column 1).
setCursorPosition :: Int -- ^ 1-based row to move to
                  -> Int -- ^ 1-based column to move to
                  -> B.Builder ()
setCursorPosition n m = csi [n, m] (B.char8 'G')

saveCursor, restoreCursor :: B.Builder ()
saveCursor    = B.char8 '\ESC' >> B.char8 '7'
restoreCursor = B.char8 '\ESC' >> B.char8 '8'

clearFromCursorToScreenEnd, clearFromCursorToScreenBeginning, clearScreen :: B.Builder ()
clearFromCursorToLineEnd, clearFromCursorToLineBeginning, clearLine :: B.Builder ()

clearFromCursorToScreenEnd       = csi [0] (B.char8 'J')
clearFromCursorToScreenBeginning = csi [1] (B.char8 'J')
clearScreen                      = csi [2] (B.char8 'J')
clearFromCursorToLineEnd         = csi [0] (B.char8 'K')
clearFromCursorToLineBeginning   = csi [1] (B.char8 'K')
clearLine                        = csi [2] (B.char8 'K')

scrollPageUp, scrollPageDown :: Int -- ^ Number of lines to scroll by
                             -> B.Builder()
scrollPageUp n   = csi [n] (B.char8 'S')
scrollPageDown n = csi [n] (B.char8 'T')

hideCursor, showCursor :: B.Builder ()
hideCursor = csi [] "?25l"
showCursor = csi [] "?25h"

-- | XTerm control sequence to set the Icon Name and Window Title.
setTitle :: T.Text  -- ^ New Icon Name and Window Title
         -> B.Builder ()
setTitle title = do
    "\ESC]0;"
    B.text (T.filter (/= '\007') title)
    B.char8 '\007'

sgr :: [Word8]  -- ^ List of sgr code for the control sequence
    -> B.Builder ()
sgr args = do
    B.char8 '\ESC'
    B.char8 '['
    B.intercalateList (B.char8 ';') B.int args
    B.char8 'm'

reset :: B.Builder ()
reset = sgr [0]

boldIntensity, faintIntensity, resetIntensity :: B.Builder ()
boldIntensity  = sgr [1]
faintIntensity = sgr [2]
resetIntensity    = sgr [22]

bold :: B.Builder () -> B.Builder ()
bold t = boldIntensity >> t >> resetIntensity

italicized, noItalicized :: B.Builder ()
italicized      = sgr [3]
noItalicized    = sgr [23]

-- | Italicize some text
italicize :: B.Builder () -> B.Builder ()
italicize t = italicized >> t >> noItalicized

singleUnderline, doubleUnderline, noUnderline :: B.Builder ()
singleUnderline = sgr [4]
doubleUnderline = sgr [21]
noUnderline   = sgr [24]

-- | Add single underline to some text
underline  :: B.Builder () -> B.Builder ()
underline t = singleUnderline >> t >> singleUnderline

slowBlink, rapidBlink, blinkOff :: B.Builder ()
-- | less than 150 per minute
slowBlink   = sgr [5]
-- | MS-DOS ANSI.SYS, 150+ per minute; not widely supported
rapidBlink  = sgr [6]
blinkOff     = sgr [25]

conceal, reveal :: B.Builder ()
-- | Aka Hide, not widely supported.
conceal = sgr [8]
reveal = sgr [28]

invert, invertOff :: B.Builder ()
-- | Swap foreground and background colors, inconsistent emulation
invert = sgr [7]
invertOff = sgr [27]

-- | Colorized some text
color :: AnsiColor -> B.Builder () -> B.Builder ()
color c t = do
    setForeground c
    t
    setDefaultForeground

-- | Colorized some text with background color
color' :: AnsiColor -> AnsiColor -> B.Builder () -> B.Builder ()
color' c1 c2 t = do
    setForeground c1
    setBackground c2
    t
    setDefaultForeground
    setDefaultBackground

-- | Colorized some text
palette :: PaletteColor -> B.Builder () -> B.Builder ()
palette c t = do
    setPaletteForeground c
    t
    setDefaultForeground

-- | Colorized some text with background color
palette' :: PaletteColor -> PaletteColor -> B.Builder () -> B.Builder ()
palette' c1 c2 t = do
    setPaletteForeground c1
    setPaletteBackground c2
    t
    setDefaultForeground
    setDefaultBackground

-- | Colorized some text
rgb :: RGBColor -> B.Builder () -> B.Builder ()
rgb c t = do
    setRGBForeground c
    t
    setDefaultForeground

-- | Colorized some text with background color
rgb' :: RGBColor -> RGBColor -> B.Builder () -> B.Builder ()
rgb' c1 c2 t = do
    setRGBForeground c1
    setRGBBackground c2
    t
    setDefaultForeground
    setDefaultBackground

setForeground, setBrightForeground, setBackground, setBrightBackground :: AnsiColor -> B.Builder ()
setForeground c       = sgr [30 + colorToCode c]
setBrightForeground c = sgr [90 + colorToCode c]
setBackground c       = sgr [40 + colorToCode c]
setBrightBackground c = sgr [100 + colorToCode c]

setPaletteForeground, setPaletteBackground :: PaletteColor -> B.Builder ()
setPaletteForeground index = sgr [38, 5, index]
setPaletteBackground index = sgr [48, 5, index]

setRGBForeground, setRGBBackground :: RGBColor -> B.Builder ()
setRGBForeground (r,g,b) = sgr [38, 2, r, g, b]
setRGBBackground (r,g,b) = sgr [48, 2, r, g, b]

setDefaultForeground, setDefaultBackground :: B.Builder ()
setDefaultForeground = sgr [39]
setDefaultBackground = sgr [49]

-- | ANSI's eight standard colors
data AnsiColor = Black
               | Red
               | Green
               | Yellow
               | Blue
               | Magenta
               | Cyan
               | White
        deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)
        deriving anyclass T.Print

colorToCode :: AnsiColor -> Word8
colorToCode c = case c of
    Black   -> 0
    Red     -> 1
    Green   -> 2
    Yellow  -> 3
    Blue    -> 4
    Magenta -> 5
    Cyan    -> 6
    White   -> 7

-- | 8-bit palette color, see https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit
type PaletteColor = Word8

-- | 24-bit RGB color
type RGBColor = (Word8, Word8, Word8)
