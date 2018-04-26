module Prob36 where

toBase2 :: Int -> [Int]
toBase2 0 = [0]
toBase2 1 = [1]
toBase2 n = (n `rem` 2) : toBase2 (n `quot` 2)

isPalindromic :: Eq a => [a] -> Bool
isPalindromic s =
    let len = length s in
        if even len then (take (len `div` 2) s) == (reverse (drop (len `div` 2) s))
        else (take (len `div` 2) s) == (reverse (drop ((len `div` 2) + 1) s))

isDoubleBasePalindromic :: Int -> Bool
isDoubleBasePalindromic n = 
    let s = show n
    in  (isPalindromic s) && (isPalindromic (toBase2 n))

doubleBasePalindromesLessThan n = [x | x <- [1..(n-1)], isDoubleBasePalindromic x]

prob36 = sum $ doubleBasePalindromesLessThan 1000000

main = do
    print $ doubleBasePalindromesLessThan 1000000
