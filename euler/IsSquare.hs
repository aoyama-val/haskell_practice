module IsSquare where

-- 平方数か判定する
isSquare :: Integer -> Bool
isSquare n =
    let sq = sqrt (fromIntegral n)
        sqInt = (floor sq)
        fl = fromIntegral sqInt
    in  fl - sq < 1e-8 && (sqInt * sqInt) == n
