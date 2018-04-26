import System.Random

loop correctAnswer = do
    input <- getLine
    let answer = (read input) :: Int
    case (compare correctAnswer answer) of
        LT -> do
            putStrLn "もっと大きい"
            loop correctAnswer
        EQ -> do
            putStrLn "正解！"
        GT -> do
            putStrLn "もっと小さい"
            loop correctAnswer

main = do
    correctAnswer <- randomRIO (1, 100) :: IO Int
    putStr "correctAnswer = "
    putStrLn $ show correctAnswer
    loop correctAnswer
