module System.Console.Questioner.ProgressBar
  where

import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import System.Console.ANSI (clearLine, setCursorColumn)
import System.IO (BufferMode(NoBuffering), stdout)

import System.Console.Questioner.Util

newtype ProgressBar = ProgressBar ThreadId

stopProgressBar :: ProgressBar -> IO ()
stopProgressBar (ProgressBar tid) = do
    killThread tid
    clearLine
    setCursorColumn 0

spinner :: Int -> String -> IO ProgressBar
spinner interval prompt = ProgressBar <$> forkIO (setup $ loop 0)
  where
    setup = hWithBufferMode stdout NoBuffering

    loop i = do
        clearLine
        setCursorColumn 0
        putStr $ " " ++ spinnerState i ++ " " ++ prompt
        threadDelay interval
        loop $ i + 1

    -- TODO - parameterize
    spinnerStates    = ["/", "-", "\\", "|"]
    spinnerStatesLen = length spinnerStates
    spinnerState i   = spinnerStates !! (i `mod` spinnerStatesLen)
