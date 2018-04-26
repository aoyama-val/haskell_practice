-- Reader モナドの使い方
--
-- 1. 戻り値の型をReaderにする（Reader 環境の型 戻り値の型）
-- 2. 関数は do 式でくるむ
-- 3. 関数の中では e <- ask の形で環境を参照できる
-- 4. 関数の中では a <- lasjd の形で他のReader型の関数を呼び出せる
-- 4. return で返す
-- 5. 呼び出す側では runReader 関数 環境 の形で呼ぶ

import Control.Monad.Reader
import Data.List

-- 環境の最初の要素に x を足す関数
addToFirstElementOfEnv :: Int -> Reader [Int] Int
addToFirstElementOfEnv x = do
    e <- ask
    return $ (head e) + x

-- 環境の２番目の要素に x を足す関数
multiToSecondElementOfEnv :: Int -> Reader [Int] Int
multiToSecondElementOfEnv x = do
    e <- ask
    return $ (e !! 1) * x

concatAsString :: Reader [Int] String
concatAsString = do
    e <- ask
    return $ intercalate "" $ map show e

hoge :: Reader [Int] (Int, Int, String)
hoge = do
    a <- addToFirstElementOfEnv 5   -- 8
    b <- multiToSecondElementOfEnv 2    -- 2
    s <- concatAsString -- "314"
    return $ (a, b, s) -- (8, 2, "314")

main = do
    print $ runReader hoge [3, 1, 4]
