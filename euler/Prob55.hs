module Prob55 where

isPalindromic :: Eq a => [a] -> Bool
isPalindromic s =
    let len = length s in
        if even len then (take (len `div` 2) s) == (reverse (drop (len `div` 2) s))
        else (take (len `div` 2) s) == (reverse (drop ((len `div` 2) + 1) s))

isPalindromicNumber :: (Show a) => a -> Bool
isPalindromicNumber n = isPalindromic (show n)

reversePlus n =
    n + (read (reverse $ show n) :: Integer)

isLychrel n =
    all (not . isPalindromicNumber) (take 50 $ tail $ iterate reversePlus n)

prob55 = length $ filter isLychrel [1..10000]

main = do
    print prob55

--iterate reversePlus 10
