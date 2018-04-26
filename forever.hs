import Control.Monad

main = do
    forever $ do
        line <- getLine
        print $ line
