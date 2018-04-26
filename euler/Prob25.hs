module Prob25 where

import qualified Data.List as List

fibo = _fibo 1 1

_fibo x y = x : _fibo y (y + x) 

digitCount x = length (show x)

prob25 :: Maybe Int
prob25 = do
    fn <- List.find (\x -> digitCount x == 1000) fibo
    i <- List.findIndex (== fn) fibo
    return $ i + 1
