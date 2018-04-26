not_divisible :: Int -> Int -> Bool
not_divisible x y = (mod x y) /= 0

not_divisible_by :: Int -> Int -> Bool
not_divisible_by y = \x -> not_divisible x y

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve(filter (not_divisible_by x) xs)

main = print(sieve(take 100 [2,3..]))
