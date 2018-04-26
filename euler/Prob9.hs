module Prob9 where

findTriplet n = [(a, b, c) | a <- [1..n], b <- [(a+1)..(n-a)], c <- [(b+1)..(n-a-b)], a + b + c == n, a^2 + b^2 == c^2]

prob9 = let (a, b, c) = head $ findTriplet 1000 in a * b * c
