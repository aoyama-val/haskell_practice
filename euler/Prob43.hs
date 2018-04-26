import qualified Data.Char as Char
import qualified Data.Vector as V
import Data.List

intToDigits :: Int -> [Int]
intToDigits n = map ((subtract 48) . Char.ord) (show n)

digitsToInt :: [Int] -> Int
digitsToInt xs = sum $ zipWith (\a -> \n -> a*10^n) (reverse xs) [0..]

slice :: Int -> Int -> [a] -> [a]
slice start len xs = (take len (drop start xs))

primes = [2,3,5,7,11,13,17]

sliceDigits :: Int -> [Int] -> Int
sliceDigits i xs = digitsToInt (slice (i-1) 3 xs)

isInterestingByI :: [Int] -> Int -> Bool
isInterestingByI xs i = (sliceDigits i xs) `rem` (primes !! (i-2)) == 0

isInteresting :: [Int] -> Bool
isInteresting xs = all id (map (isInterestingByI xs) [8,7..2])

quickCheck :: [Int] -> Bool
quickCheck xs = even (xs !! 3) && ((xs !! 5) == 0 || (xs !! 5) == 5)

main = do
    print $ [digitsToInt perm | perm <- permutations [0,1,2,3,4,5,6,7,8,9], isInteresting perm]
