-- メモ化のサンプル
--
-- stack install data-memocombinators
-- http://hackage.haskell.org/package/data-memocombinators-0.4.3/docs/Data-MemoCombinators.html


import qualified Data.MemoCombinators as Memo

fib = Memo.integral fib'
    where
        fib' 0 = 0
        fib' 1 = 1
        fib' x = fib (x-1) + fib (x-2)

main = print $ fib 100
