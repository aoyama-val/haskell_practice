module Prob17 where

numberToLetters :: Int -> String
numberToLetters 0 = ""
numberToLetters 1 = "one"
numberToLetters 2 = "two"
numberToLetters 3 = "three"
numberToLetters 4 = "four"
numberToLetters 5 = "five"
numberToLetters 6 = "six"
numberToLetters 7 = "seven"
numberToLetters 8 = "eight"
numberToLetters 9 = "nine"
numberToLetters 10 = "ten"
numberToLetters 11 = "eleven"
numberToLetters 12 = "twelve"
numberToLetters 13 = "thirteen"
numberToLetters 14 = "fourteen"
numberToLetters 15 = "fifteen"
numberToLetters 16 = "sixteen"
numberToLetters 17 = "seventeen"
numberToLetters 18 = "eighteen"
numberToLetters 19 = "nineteen"
numberToLetters 20 = "twenty"
numberToLetters 30 = "thirty"
numberToLetters 40 = "forty"
numberToLetters 50 = "fifty"
numberToLetters 60 = "sixty"
numberToLetters 70 = "seventy"
numberToLetters 80 = "eighty"
numberToLetters 90 = "ninety"
numberToLetters 100 = "one hundred"
numberToLetters 1000 = "one thousand"
numberToLetters n
    | n `mod` 100 == 0 = (numberToLetters (n `div` 100)) ++ " hundred"
    | n >= 100 = numberToLetters (n `div` 100) ++ " hundred and " ++ numberToLetters (n `mod` 100)
    | otherwise = numberToLetters (n `div` 10 * 10) ++ " " ++ numberToLetters (n `mod` 10)

numberLetterLength :: Int -> Int
numberLetterLength = length . filter (/= ' ') . numberToLetters

lettersCount n = sum $ map numberLetterLength [1..n]

prob17 = lettersCount 1000
