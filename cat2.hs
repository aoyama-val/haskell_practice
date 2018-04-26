-- sequenceを使う実装

import System.IO
import System.Environment

main = do
    args <- getArgs
    contents <- sequence $ map readFile args
    putStr $ foldl (++) "" contents
