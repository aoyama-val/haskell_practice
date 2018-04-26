module Prob38 where

import Data.List

concatMultiples n m = foldl (++) "" (map (show . (n *)) [1..m])

isPandigital s = sort s == "123456789"

list = takeWhile ((<= 9) . length) [concatMultiples x 3 | x <- [1..]]

hoge9 = [(x, y) | x <- [9..9], y <- [1..9], isPandigital (concatMultiples x y)]

-- 2桁なら y <= 4
hoge90 = [(x, y) | x <- [90..99], y <- [2..4], isPandigital (concatMultiples x y)]

-- 3桁なら y <= 3
hoge900 = [(x, y) | x <- [100..999], y <- [2..3], isPandigital (concatMultiples x y)]

-- 4桁なら y <= 2 のはず
hoge9000 = [(x, y) | x <- [1000..9999], y <- [2..2], isPandigital (concatMultiples x y)]

prob38 = maximum $ map (\(x, y) -> (read (concatMultiples x y) :: Int)) (hoge9 ++ hoge90 ++ hoge900 ++ hoge9000)
