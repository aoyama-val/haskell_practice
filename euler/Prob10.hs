--module Prob10 where

import Sieve

prob10 = sum $ sieve 2000000

main = print prob10
