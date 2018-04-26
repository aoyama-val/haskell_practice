import Control.Monad.Reader

hoge :: Int -> Reader [Int] Int
hoge n = do
    x <- ask    -- x == [3, 4, 5]
    y <- moge (x !! 1)  -- moge 4   == 12
    return $ y + n  -- 12 + 10

moge :: Int -> Reader [Int] Int
moge n = do
    x <- ask    -- [3, 4, 5]
    return $ head x * n -- 3 * 4

main = do
    print $ runReader (hoge 10) [3, 4, 5]

