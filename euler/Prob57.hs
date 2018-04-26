import Data.Ratio
import Data.List
import qualified Data.MemoCombinators as Memo

d :: Int -> Ratio Integer
d = Memo.integral d'
    where
        d' 0 = 1 + (1 % 2)
        d' n = 1 + 1 / (1 + d (n-1))

numGTdenom frac = (length $ show $ numerator frac) > (length $ show $ denominator frac)

prob57 = length $ filter numGTdenom [d n | n <- [0..999]]
