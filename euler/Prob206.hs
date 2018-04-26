import Data.List

sq n = (read ("1" ++ (show n) ++ "0") :: Integer)^2

try n = isMatch $ show $ sq n

isMatch s = let masked = "1_2_3_4_5_6_7_8_9_0"
            in  and $ zipWith (\c1 -> \c2 -> c1 == '_' || c1 == c2) masked s

prob206 = find try [10000000..99999999]

main = do
    print prob206
