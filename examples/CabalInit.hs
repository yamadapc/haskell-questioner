{-# LANGUAGE LambdaCase #-}
import Data.Maybe
import Text.Printf
import System.Console.Questioner
import System.Directory
import System.FilePath
import System.IO

prompt' :: String -> String -> IO String
prompt' message defaultValue = do
    answer <- prompt (printf "%s [default: %s]" message defaultValue :: String)
    return $ fromMaybe defaultValue answer

listPromptWithOther :: String -> [String] -> IO String
listPromptWithOther question options = listPrompt question options >>= \case
    "Other (specify)" -> prompt question
    s -> return s

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    pkgName    <- getCurrentDirectory >>= promptName
    pkgVersion <- promptVersion
    pkgLicense <- promptLicense
    pkgAuthor <- prompt "Author name? "
    pkgMaintainer <- prompt "Maintainer email? "
    pkgHomepage <- prompt "Project homepage URL? "
    pkgSynopsis <- prompt "Project synopsis? "
    pkgCategory <- listPromptWithOther "Project category? " categories
    pkgType <- listPrompt "What does your package build? " [ "Library"
                                                           , "Executable"
                                                           ]
    putStrLn pkgName
    putStrLn pkgVersion
    putStrLn pkgLicense
    putStrLn pkgAuthor
    putStrLn pkgMaintainer
    putStrLn pkgHomepage
    putStrLn pkgSynopsis
    putStrLn pkgCategory
    putStrLn pkgType
    putStrLn "Have fun :)"
  where
    promptName cwd = prompt' "Package name? " (takeFileName cwd)
    promptVersion = prompt' "Package version?" "0.1.0.0"
    promptLicense = listPrompt "Please choose a license:" licenses

licenses :: [String]
licenses = [ "(none)"
           , "GPL-2"
           , "GPL-3"
           , "LGPL-2.1"
           , "LGPL-3"
           , "AGPL-3"
           , "BSD2"
           , "BSD3"
           , "MIT"
           , "ISC"
           , "MPL-2.0"
           , "Apache-2.0"
           , "PublicDomain"
           , "AllRightsReserved"
           , "Other (specify)"
           ]

categories :: [String]
categories = [ "(none)"
             , "Codec"
             , "Concurrency"
             , "Control"
             , "Data"
             , "Database"
             , "Development"
             , "Distribution"
             , "Game"
             , "Graphics"
             , "Language"
             , "Math"
             , "Network"
             , "Sound"
             , "System"
             , "Testing"
             , "Text"
             , "Web"
             , "Other (specify)"
             ]
