{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
module System.Console.Questioner
    (
      Question(..)

    , ChoiceEvent
    , charToChoiceEvent
    , listPrompt
    , checkboxPrompt

    , module System.Console.Questioner.ProgressIndicators
    )
  where

import Control.Applicative ((<$>))
import Control.Monad ((>=>), forM_)
import Data.List (delete)
import System.Console.ANSI (SGR(..), Color(..), ColorIntensity(..),
                            ConsoleLayer(..), clearLine, cursorUpLine, setSGR)
import System.IO (stdin)

import System.Console.Questioner.ProgressIndicators
import System.Console.Questioner.Util

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

instance Question (String, [String]) [String] where
    prompt = uncurry checkboxPrompt

-- Multiple choice prompts
-------------------------------------------------------------------------------

data ChoiceEvent = MoveUp | MoveDown | MakeChoice | ToggleSelection
  deriving(Eq, Ord, Show)

charToChoiceEvent :: Char -> Maybe ChoiceEvent
charToChoiceEvent 'j'  = Just MoveDown
charToChoiceEvent 'k'  = Just MoveUp
charToChoiceEvent '\n' = Just MakeChoice
charToChoiceEvent ' '  = Just ToggleSelection
charToChoiceEvent _    = Nothing

listPrompt :: String -> [String] -> IO String
listPrompt question options = setup $ do
    putStrLn question
    -- selection has structure: (selected item's index, indexed options)
    let selection = (0, zip options ([0..] :: [Int]))
    render selection
    i <- listenForSelection selection
    return $ options !! i
  where
    setup = hWithNoBuffering stdin . withNoEcho

    listenForSelection os = charToChoiceEvent <$> getChar >>= \case
        Nothing -> listenForSelection os
        Just ToggleSelection -> listenForSelection os

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

    updateSelection MoveUp   (i, os) = ((i - 1) `mod` length os, os)
    updateSelection MoveDown (i, os) = ((i + 1) `mod` length os, os)
    updateSelection _ _ = error "Internal error, key not recognized"

    render (s, optionsI) = forM_ optionsI $ \(o, i) ->
        if i == s
            then do
                setSGR [ SetColor Foreground Vivid Blue ]
                putStrLn $ "> " ++ o
                setSGR []
            else putStrLn $ "  " ++ o

        -- putStrLn $ (if i == s then "> " else "  ") ++ o

checkboxPrompt :: String -> [String] -> IO [String]
checkboxPrompt question options = setup $ do
    putStrLn question
    let selection = (0, [], zip options ([0..] :: [Int]))
    render selection
    is <- listenForSelection selection
    return $ map (options !!) is
  where
    setup = hWithNoBuffering stdin . withNoEcho

    listenForSelection o = charToChoiceEvent <$> getChar >>= \case
        Just MakeChoice -> do
            let (_, _, optionsI) = o in
                forM_ (replicate (length optionsI + 1) ())
                      (const (clearLine >> cursorUpLine 1))

            clearLine
            let (_, is, _) = o in
                return is

        Just d -> do
            let (_, _, optionsI) = o in
                clearFromCursorTo $ length optionsI
            let o' = updateSelection d o in
                render o' >> listenForSelection o'
        Nothing -> listenForSelection o

    updateSelection MoveUp   (i, is, os) = ((i - 1) `mod` length os, is, os)
    updateSelection MoveDown (i, is, os) = ((i + 1) `mod` length os, is, os)
    updateSelection ToggleSelection (i, is, os) = (i, is', os)
      where
        is' = if i `elem` is then delete i is else i:is
    updateSelection _ _ = error "Internal error, key not recognized"

    render (i, is, optionsI) = forM_ optionsI $ \(o, j) -> do
        let checkbox = if j `elem` is then "◉ " else "◯ "
        if i == j
            then do
                setSGR [ SetColor Foreground Vivid Blue ]
                putStrLn $ ">" ++ checkbox ++ o
                setSGR []
            else putStrLn $ " " ++ checkbox ++ o
