import qualified Data.Char as Char
import Data.Ord
import Data.List
import Data.Char

caesar :: Int -> String -> String
caesar n s = map (rot n) s

rot :: Int -> Char -> Char
rot _ ' ' = ' '
rot n c = Char.chr $ ((Char.ord c + n) - 97) `mod` 26 + 97

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: (Eq a) => a -> [a] -> Int
count x xs = length [y | y <- xs, y == x]

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | let n = lowers xs, x <- ['a'..'z']]

chisqr :: [Float] -> [Float] -> Float
chisqr xs ys = sum [(sqrt ((x-y)^2)) / y | (x, y) <- zip xs ys]

rotate n xs = (drop n xs) ++ (take n xs)

lowers xs = length [x | x <- xs, isLower x]


maxi xs = maximumBy (comparing fst) (zip xs [0..])
mini xs = minimumBy (comparing fst) (zip xs [0..])

main = do
    print $ caesar 3 "haskell is fun"
    --print $ count 'o' "hogehogehoge"
    --print $ freqs "this is a pen"
    let table' = freqs "kdvnhoo lv ixq"
    print $ table'
    print $ [chisqr (rotate n table') table | n <- [0..25]] 
    print $ mini [chisqr (rotate n table') table | n <- [0..25]] 
