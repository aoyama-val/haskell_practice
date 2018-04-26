lastTenDigits n m
    | m == 1    = n `rem` 10000000000
    | otherwise = n * (lastTenDigits n (m-1)) `rem` 10000000000

prob48 = (sum [lastTenDigits n n | n <- [1..1000]]) `rem` 10000000000

main = do
    print prob48
