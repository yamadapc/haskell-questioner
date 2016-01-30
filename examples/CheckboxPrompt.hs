import           System.Console.Questioner

main :: IO ()
main = do
    fs <- prompt ( "What features would you like included?"
                 , [ "Testing" , "Makefile", "guard support" ] ) :: IO [String]
    putStrLn "Your choices:"
    mapM_ putStrLn fs
