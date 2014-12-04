import Control.Concurrent
import System.Console.Questioner

main :: IO ()
main = do
    s <- spinner (1000 * 200) "Loading..."
    threadDelay (1000 * 10000) -- 10s
    stopProgressBar s
    putStrLn "Done!"
