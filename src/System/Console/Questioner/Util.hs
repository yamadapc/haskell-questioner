module System.Console.Questioner.Util
  where

import Control.Exception (bracket_)
import System.Console.ANSI (clearLine, cursorDownLine, cursorUpLine,
                            hideCursor, showCursor)
import System.IO (BufferMode(..), stdin, hGetBuffering, hSetBuffering, hSetEcho)

-- |
-- Performs an IO action with NoBuffering on standard input
withNoBuffering :: IO a -> IO a
withNoBuffering action = do
    originalBuffering <- hGetBuffering stdin
    bracket_
        (hSetBuffering stdin NoBuffering)
        (hSetBuffering stdin originalBuffering)
        action

-- |
-- Performs an IO action with the console cursor hidden
withNoCursor :: IO a -> IO a
withNoCursor = bracket_ hideCursor showCursor

-- |
-- Performs an IO action with console "echoing" supressed
withNoEcho :: IO a -> IO a
withNoEcho = bracket_ (hSetEcho stdin False) (hSetEcho stdin True)

-- |
-- Clears the screen from the cursor's current position until `n` lines
-- above it
clearFromCursorTo :: Int -> IO ()
clearFromCursorTo nlines = loop nlines >> cursorDownLine (nlines - 2)
  where
    loop (-1) = return ()
    loop n = clearLine >> cursorUpLine 1 >> loop (n - 1)
