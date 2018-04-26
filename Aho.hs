module Aho where

import Data.List
import Data.Maybe

cycleMod m n = m : cycleMod (10 * (m `mod` n)) n

cycledPart :: Eq a => [a] -> [a]
cycledPart = head . filter (\l -> (length . nub) l /= length l) . inits

periodLength n = length initPart - fromJust (last initPart `elemIndex` initPart) - 1 where initPart = cycledPart (cycleMod 1 n)

result = snd $ maximum $ zip (map periodLength [3,5..999]) [3,5..]

fibo  = _fibo 1 1

_fibo x y = x : _fibo y x + y

main = print $ result
