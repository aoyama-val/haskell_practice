module Prob20 where

import Data.Char as Char

fac n = product [1..n]

charToDigit c = Char.ord c - 48

prob20 = sum $ map charToDigit (show (fac 100))
