module Prob6 where

square n = n * n

sumOfSquares :: [Int] -> Int
sumOfSquares xs = sum $ map square xs

squareOfSum :: [Int] -> Int
squareOfSum xs = square $ sum xs

prob6 = 
    let xs = [1..100]
    in  (squareOfSum xs) - (sumOfSquares xs)
