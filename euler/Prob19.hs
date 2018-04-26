module Prob19 where

isLeapYear n = (n `mod` 4 == 0) && (n `mod` 100 /= 0 || n `mod` 400 == 0)

days n = [31, 28 + (if isLeapYear n then 1 else 0), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

daysInYear year = sum $ days year

dayCountFromJan1900 y m d = (sum $ map daysInYear [1900..(y-1)])+ (sum $ take (m - 1) (days y)) + (d - 1)

wday y m d = (1 + dayCountFromJan1900 y m d) `mod` 7

prob19 = length [(y, m) | y <- [1901..2000], m <- [1..12], wday y m 1 == 0]
