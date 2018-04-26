module Prob34 where

import qualified Data.Char as Char
import qualified Data.MemoCombinators as Memo

fac = Memo.integral fac'
    where
        fac' n = product [1..n]

intToDigits :: Int -> [Int]
intToDigits n = map ((subtract 48) . Char.ord) (show n)

isCurious :: Int -> Bool
isCurious n = n == (sum $ map fac (intToDigits n))

prob34 = [n | n <- [3..9999999], isCurious n]
