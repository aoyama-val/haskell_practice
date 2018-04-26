import qualified Data.Char as Char
import Data.Function (on)
import Data.List

intToDigits :: (Show a) => a -> [Int]
intToDigits n = map ((subtract 48) . Char.ord) (show n)

digitsSum n = sum $ intToDigits n

p x = digitsSum $ fst x

prob56 = maximumBy (compare `on` p) [(a^b, (a, b)) | a <- [2..99], b <- [2..99]]
