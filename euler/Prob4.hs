module Prob4 where

slice from to xs = take (to - from + 1) (drop from xs)

isPalindromic :: String -> Bool
isPalindromic s =
    let len = length s in
        if even len then (take (len `div` 2) s) == (reverse (drop (len `div` 2) s))
        else (take (len `div` 2) s) == (reverse (drop ((len `div` 2) + 1) s))

prob4 = maximum [(x * y) | x <- [100..999], y <- [100..x], isPalindromic (show $ x * y)]
