-- cat -nコマンド
-- しかし複数ファイル指定した場合、各ファイルごとに行ナンバーがついてしまう（cat -nとは異なる）

import System.IO
import System.Environment

leftPad str len padStr =
    if (length str) >= len then
        str
    else
        (repeatString padStr (len - (length str))) ++ str

repeatString str len =
    if (len == 0) then
        ""
    else
        str ++ (repeatString str (len - 1))

addLineNumber num line =
    (leftPad (show num) 6 " ") ++ "  " ++ line

readFileToList filename = do
    content <- readFile filename
    let lis = lines content
    return lis

catN filename = do
    lns <- readFileToList filename
    mapM putStrLn (zipWith addLineNumber [1..] lns)

main = do
    args <- getArgs
    mapM catN args
