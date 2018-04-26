module Prob67 where

-- Prob18の大量データ版
{-
(i, j) に対し、(i, j)に来るまでの最大の経路を計算できるようにすればよい

findMaxPath (i, j) = max (findMaxPath (i - 1, j - 1)) (findMaxPath (i - 1, j))

メモ化して動的計画法で。
ハッシュテーブルを使えばよいか
-}
