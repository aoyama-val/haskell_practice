module Prob29 where

import qualified Data.List as List
prob29 = List.nub [a^b | a <- [2..100], b <- [2..100]]
