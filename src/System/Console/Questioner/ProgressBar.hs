module System.Console.Questioner.ProgressBar
  where

import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import System.Console.ANSI (clearLine, setCursorColumn)

import System.Console.Questioner.Util

newtype ProgressBar = ProgressBar ThreadId

stopProgressBar :: ProgressBar -> IO ()
stopProgressBar (ProgressBar tid) = do
    killThread tid
    clearLine

spinner :: Int -> String -> IO ProgressBar
spinner interval prompt = ProgressBar <$> forkIO (withNoCursor $ loop 0)
  where
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
