forever action = do
    x <- action
    forever action

main = do
    forever $ do
        putStrLn "hello"
