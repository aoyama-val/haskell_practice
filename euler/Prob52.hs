import qualified Data.Char as Char
import Data.List

-- 17 * 6 で3桁になってしまうため、上2桁が10~17となる数だけ探せば良い
getRange n = [10*10^(n-1)..17*10^(n-1)]

intToDigits :: Int -> [Int]
intToDigits n = map ((subtract 48) . Char.ord) (show n)

digitsToInt :: [Int] -> Int
digitsToInt xs = sum $ zipWith (\a -> \n -> a*10^n) (reverse xs) [0..]

sortedDigits n = sort $ (show n)

isAllSameDigits n =
    all (== (sortedDigits n)) [sortedDigits (n*x) | x <- [2..6]]

-- とりあえず getRange 5 の範囲で探したらあっさり見つかってしまった
prob52 = find isAllSameDigits (getRange 5)

