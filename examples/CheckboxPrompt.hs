import System.Console.Questioner

main :: IO ()
main = do
    fs <- prompt ( "What features would you like included?"
                 , [ "Testing" , "Makefile", "guard support" ] )
    putStrLn "Your choices:"
    mapM_ putStrLn fs
