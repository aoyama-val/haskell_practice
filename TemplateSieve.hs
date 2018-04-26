-- Template Haskellを使って定数をコンパイル時に計算しておく
-- http://qiita.com/aosho235/items/62ca7e5b3135e1f3122d

{-# LANGUAGE TemplateHaskell #-}

import SlowSieve hiding (main)

main = print $( let x = sieve [2..100000] in [| x |] )
