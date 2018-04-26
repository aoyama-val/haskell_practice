import System.IO
import System.Environment

cat filename = do
    content <- readFile filename
    putStr content

main = do
    args <- getArgs
    mapM cat args
