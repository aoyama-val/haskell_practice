--module Prob33 where

type Fraction = ([Int], [Int])

cancel :: Fraction -> Fraction
cancel (xs, ys) = ([x | x <- xs, x `notElem` ys], [y | y <- ys, y `notElem` xs])

intListToInt :: [Int] -> Int
intListToInt xs = sum $ zipWith (*) (reverse xs) [10^n | n <- [0..]]

lessThanOne :: Fraction -> Bool
lessThanOne (xs, ys) = (intListToInt xs) < (intListToInt ys)

rationalEqual :: Fraction -> Fraction -> Bool
rationalEqual (xs, ys) (us, vs) = (intListToInt xs) * (intListToInt vs) == (intListToInt ys) * (intListToInt us)


isCurious :: Fraction -> Bool
isCurious f@(xs, ys) = (not $ null xs) && (not $ null ys) && (not $ null (fst canceled)) && (not $ null (snd canceled)) && f /= canceled && lessThanOne f && rationalEqual f canceled
                    where
                        canceled = cancel f

--lessThanOne :: ([Int], [Int]) -> 
--lessThanOne (xs, ys) = (intListToInt xs) < (intListToInt ys)

isProper :: [Int] -> Bool
isProper xs = xs /= [0, 0]

prob33 = [([a, b], [c, d]) | a <- [1..9], b <- [0..9], c <- [1..9], d <- [0..9], not (b == 0 && d == 0), isCurious ([a, b], [c, d])]
