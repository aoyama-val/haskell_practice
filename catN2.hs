-- cat -nコマンド
-- しかし複数ファイル指定した場合、行ナンバーリセットされない（cat -nと同じ）

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

catStrings strings =
    foldr (++) "" strings

-- 実装1(mapMを使う。mapMがよく分からない）
catFiles filenames = do
    contents <- mapM readFile filenames
    return (catStrings contents)

-- 実装2（再帰を使う）
--catFiles [] = do
    --return ""
--catFiles (x:xs) = do
    --content <- readFile x
    --content2 <- catFiles xs
    --return $ content ++ content2

main = do
    args <- getArgs
    putStrLn =<< catFiles args
