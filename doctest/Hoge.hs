module Hoge where

{-| エンコードする

>>> encode "hoge"
"mogehoge"

>>> encode "sage"
"failtest"

-}
encode :: String -> String
encode s = "moge" ++ s

main = do
    let a = "hogehoge"
    print $ encode a
