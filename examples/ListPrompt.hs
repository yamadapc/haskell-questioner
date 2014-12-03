import System.Console.Questioner

main :: IO ()
main = do
    e <- prompt ("What's your editor?", ["Vim", "Emacs", "Sublime Text"])
    putStrLn $ e ++ "?! " ++
               if e == "vim" then "That's very cool!" else "That's not so cool!"
