-- comm コマンドの実装
-- パフォーマンスは全然考えていない

import Data.List
import System.Environment
import GHC.Exts

onlyLeft :: Eq a => [a] -> [a] -> [a]
onlyLeft xs ys = [x | x <- xs, not $ x `elem` ys]

onlyRight :: Eq a => [a] -> [a] -> [a]
onlyRight xs ys = [y | y <- ys, not $ y `elem` xs]

common :: Eq a => [a] -> [a] -> [a]
common xs ys = [x | x <- xs, x `elem` ys]

formatPair :: (String, Int) -> String
formatPair (s, i) | i == 1 = s
formatPair (s, i) | i == 2 = "\t" ++ s
formatPair (s, i) | i == 3 = "\t\t" ++ s

printSorted :: [(String, Int)] -> IO ()
printSorted pairs = putStr $ unlines $ map formatPair pairs

main :: IO ()
main = do
    args <- getArgs
    content1 <- readFile (args !! 0)
    let xs = lines content1
    content2 <- readFile (args !! 1)
    let ys = lines content2
    let left   = map (\x -> (x, 1)) (onlyLeft xs ys)
    let right  = map (\x -> (x, 2)) (onlyRight xs ys)
    let comm = map (\x -> (x, 3)) (common xs ys)
    let sorted = sortWith fst (left ++ right ++ comm)
    printSorted sorted
