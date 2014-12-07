import Control.Concurrent
import System.Console.Questioner

main :: IO ()
main = do
    p <- simpleProgressBar
    loop p 0
  where
    loop p i = do
        threadDelay (1000 * 100) -- 0.1s
        let i' = i + 0.1
        if i >= 1
            then stopIndicator p >> putStrLn "Done!"
            else updateIndicator p i' >> loop p i'
