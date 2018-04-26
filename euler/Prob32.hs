module Prob32 where

-- 桁数を考えると、
-- あり得る 1 x 4 = 4
-- あり得る 2 x 3 = 4
-- あり得る 3 x 2 = 4   上のと同じ
-- あり得る 4 x 1 = 4   上のと同じ
-- ありえない 1 x 3 = 5
-- ありえない 2 x 2 = 5
-- ありえない 1 x 2 = 6
-- ありえない 1 x 1 = 7
--
-- よって 1 x 4 と 2 x 3 の場合だけ探せば良い


--import Combi
import Data.List
import qualified Data.Set as Set

digits = "123456789"

-- 重複を許してn個の列を作る
-- >>> permutateD "abcd" 2
-- ["aa","ab","ac","ad","ba","bb","bc","bd","ca","cb","cc","cd","da","db","dc","dd"]
permutateD :: [a] -> Int -> [[a]]
permutateD [] _ = []
permutateD items 0 = [[]]
permutateD items r = concat $ map (\i -> map (i :) (permutateD items (r - 1))) items

-- 重複を許さずn個の列を作る
permutate :: Eq a => [a] -> Int -> [[a]]
permutate [] _ = []
permutate items 0 = [[]]
permutate items r = concat $ map (\i -> map (i :) (permutate (filter (/= i) items) (r - 1))) items

isMultiPandigital :: String -> String -> Bool
isMultiPandigital x y =
    let sm = stringMulti x y
        s = x ++ y ++ (show sm)
    in  isPandigital s

isPandigital s = (length s) == 9 && notElem '0' s && (length $ nub s) == 9

--multiDigitCount :: String -> String -> Int
--multiDigitCount x y = digitCount $ filter (/= '0') $ x ++ y ++ show (stringMulti x y)

--digitCount :: String -> Int
--digitCount s = Set.size $ Set.fromList s

stringMulti :: String -> String -> Int
stringMulti x y = (read x :: Int) * (read y :: Int)

pandigitals =
    let perms = [(p1, p2) | p1 <- permutate digits 2, p2 <- permutate (filter (\x -> notElem x p1) digits) 3]
        ans1 =  [(p1, p2, stringMulti p1 p2) | p@(p1, p2) <- perms, isMultiPandigital p1 p2]
        perms2 = [(p1, p2) | p1 <- permutate digits 1, p2 <- permutate (filter (\x -> notElem x p1) digits) 4]
        ans2 =  [(p1, p2, stringMulti p1 p2) | p@(p1, p2) <- perms2, isMultiPandigital p1 p2]
    in  [p3 | (p1, p2, p3)  <- ans1 ++ ans2]

prob32 = sum $ nub pandigitals
