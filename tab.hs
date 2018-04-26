-- column -ts, のHaskell版
-- CSVを整形して表示する

import System.Environment
import Data.List.Split
import Data.List
import Data.Ord

-- 2次元リストで、要素数が一番多い行の要素数を返す
maxLen :: [[a]] -> Int
maxLen lists = maximum $ map length lists

-- 正方行列にする
normalize :: [[String]] -> [[String]]
normalize lines =
    let maxlen = maxLen lines
    in  [take maxlen (line ++ repeat "") | line <- lines]

-- 2次元リストで、指定の列を取得する
getColumn :: [[a]] -> Int -> [a]
getColumn mat i = map (\row -> row !! i) mat

-- リストの各要素に関数を適用し、その値で最大値を探す。ペアを返す。
maximumByFunc :: Ord a => (a1 -> a) -> [a1] -> (a1, a)
maximumByFunc f xs = maximumBy (comparing snd) (zip xs (map f xs))

-- 右埋め
rpad :: [a] -> Int -> a -> [a]
rpad xs len x = take len (xs ++ repeat x)

-- スペース右埋めによって全ての要素の長さを揃える
padFields :: [String] -> [String]
padFields fields =
    let maxItem = maximumByFunc length fields
    in  map (\x -> rpad x (snd maxItem) ' ') fields

tab :: [String] -> [String]
tab lines =
    let fieldss = map (splitOn ",") lines
        normalized = normalize fieldss
        transposed = transpose normalized
        padded = map padFields transposed
    in  map (intercalate "\t") (transpose padded)

main = do
    contents <- getContents
    putStr $ unlines $ tab $ lines $ contents
