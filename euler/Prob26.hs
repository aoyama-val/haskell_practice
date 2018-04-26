-- この問題は、割り算を実装するのはそこまででもないが、循環を検出するのが難しかった

module Prob26 where

import qualified Data.List as List
import qualified Data.Function as Function

-- 10のべき乗でn以上になる最小の数を返す
floor10 n = floor10' n 1
    where
        floor10' n tmp =
            if n > tmp then floor10' n (10 * tmp)
            else    tmp

-- 割ってみる（各桁の配列を返す）
divide a n =
    if a >= n
    then let q = a `div` n
             r = a `mod` n
         in  if r == 0
             then [q]
             else q : (divide (10 * r) n)
    else 0 : divide (10 * a) n

cycleLength n = cycleLength' (floor10 n) n []
        
cycleLength' :: Integer -> Integer -> [Integer] -> Int
cycleLength' a n rs =
    if a >= n
    then let q = a `div` n
             r = a `mod` n
         in  if r == 0
             then 0
             else case List.findIndex (== r) rs of
                      Just i -> length rs
                      Nothing -> (cycleLength' (10 * r) n (r : rs))
    else 1 + cycleLength' (10 * a) n rs

prob26 = List.maximumBy (compare `Function.on` cycleLength) [2..999]

main = print $ prob26
