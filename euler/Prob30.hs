module Prob30 where

import qualified Data.Char as Char

numToDigits :: Integer -> [Int]
numToDigits = (map  ((subtract 48) . Char.ord)) . show 

powers = sum . (map (\n -> n^5)) . numToDigits

prob30 = [n | n <- [2..999999], powers n == fromInteger n]
