module Prob42 where

import Data.Char
import Data.List.Split

wordValue :: String -> Int
wordValue s = sum $ map ((subtract 64) . ord) s

triangleNumbers :: [Int]
triangleNumbers = zipWith (+) [1..] (0 : triangleNumbers)

isTriangleNumber :: [Int] -> Int -> Bool
isTriangleNumber list num = num `elem` list

prob42 list words = [w | w <- words, isTriangleNumber list (wordValue w)]

main = do
    content <- readFile "p042_words.txt"
    let strings = splitOn "," content
    let words = map (read :: String -> String) strings
    let triangleNumberList = takeWhile (< 520) triangleNumbers
    print $ triangleNumberList
    print $ isTriangleNumber triangleNumberList 55
    print $ length $ prob42 triangleNumberList words
