{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
module System.Console.Questioner
  where

import Control.Applicative ((<$>))
import Control.Exception (bracket_)
import Control.Monad ((>=>), forM_)
import System.Console.ANSI
import System.IO (BufferMode(..), stdin, hSetBuffering, hSetEcho)

-- Base `Question` and `Question` instances
-------------------------------------------------------------------------------

class Question q a where
    prompt :: q -> IO a

instance Read a => Question String a where
    prompt = putStr . (++ " ") >=> const readLn

instance Question String String where
    prompt = putStr . (++ " ") >=> const getLine

instance Question (String, (String, String)) String where
    prompt (s, (o1, o2)) = do
        putStr s
        putStr $ " (" ++ o1 ++ "/" ++ o2 ++ ") "
        getLine

instance Question (String, [String]) String where
    prompt = uncurry listPrompt

-- Multiple choice prompts
-------------------------------------------------------------------------------

data ChoiceEvent = MoveUp | MoveDown | MakeChoice
  deriving(Eq, Ord, Show)

charToChoiceEvent :: Char -> Maybe ChoiceEvent
charToChoiceEvent 'j'  = Just MoveDown
charToChoiceEvent 'k'  = Just MoveUp
charToChoiceEvent '\n' = Just MakeChoice
charToChoiceEvent _    = Nothing

listPrompt :: String -> [String] -> IO String
listPrompt question options = withNoBuffering $ withNoEcho $ withNoCursor $ do
    putStrLn question
    -- selection has structure: (selected item's index, indexed options)
    let selection = (0, zip options ([0..] :: [Int]))
    render selection
    i <- listenForSelection selection
    return $ options !! i
  where
    listenForSelection os = getDirection >>= \case
        Just MakeChoice -> do
            forM_ (replicate (length (snd os) + 1) ())
                  (const (clearLine >> cursorUpLine 1))
            clearLine
            return $ fst os

        Just d -> do
            let os' = updateSelection d os
            clearFromCursorTo $ length $ snd os
            render os'
            listenForSelection os'

        Nothing -> listenForSelection os

    updateSelection MoveUp   (i, os) = ((i - 1) `mod` length os, os)
    updateSelection MoveDown (i, os) = ((i + 1) `mod` length os, os)
    updateSelection _ _ = error "Internal error, key not recognized"

    render (s, indexedOptions) = forM_ indexedOptions $ \(o, i) ->
        putStrLn $ (if i == s then "> " else "  ") ++ o

-- Utility functions
-------------------------------------------------------------------------------

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
-- Reads a "direction" from the terminal; doesn't currently parse escape
-- sequences
getDirection :: IO (Maybe ChoiceEvent)
getDirection = charToChoiceEvent <$> getChar

-- |
-- Clears the screen from the cursor's current position until `n` lines
-- above it
clearFromCursorTo :: Int -> IO ()
clearFromCursorTo nlines = loop nlines >> cursorDownLine (nlines - 2)
  where
    loop (-1) = return ()
    loop n = clearLine >> cursorUpLine 1 >> loop (n - 1)
