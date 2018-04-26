module Prob39 where

import qualified Data.List as List
import qualified Data.Function as Function

findSides n = [(a, b, c) | a <- [2..(n-2)], b <- [a..(n-a-1)], 2 * (a+b) >= n, let c = n-a-b, a^2+b^2==c^2]

main = print $ List.maximumBy (compare `Function.on` snd) [(n, sideCount) | n <- [1000,999..120], let sideCount = length (findSides n)]
--main = print $ findSides 1000
