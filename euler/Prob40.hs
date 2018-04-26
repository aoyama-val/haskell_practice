--module Prob40 where

import Data.Char

part n = n * 9 * 10^(n-1)

-- tail . (map sum) . inits
-- でも同じだった
sumSeq :: Num a => [a] -> a -> [a]
sumSeq [] _ = []
sumSeq (x:xs) accum = (x + accum) : (sumSeq xs (x + accum))
--
partSeq = sumSeq (map part [1..]) 0

prevPart n = takeWhile (<= n) partSeq

--partSeq :: [(Int, Int)]
--partSeq [] _ = []
--partSeq (x:xs) accum =

d :: Int -> Int
d 1 = 1
d n = let prev_part = prevPart n
          prevLen = length prev_part
          nextStart = 10^prevLen
          lastNum = last prev_part
      in  ord ((concat $ map show [nextStart..]) !! (n - lastNum - 1)) - 48

prob40 = product $ map d [1, 10, 100, 1000, 10000, 100000, 1000000]

--main = print $ prob40
