module Combi where

-- >>> crossProductNTime [1..3] 2
-- [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
crossProductNTime :: [a] -> Int -> [[a]]
crossProductNTime xs 1 = [[x] | x <- xs]
crossProductNTime xs n = [x : xs' | x <- xs, xs' <- (crossProductNTime xs (n - 1))]

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
