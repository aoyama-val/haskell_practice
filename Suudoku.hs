-- 数独ソルバー
-- 関数プログラミング実践入門より

import Data.List
import Data.Function

-- (横, 縦) の座標で表す
type Cell = (Int, Int)

type Board = [(Cell, Int)]

allCells :: [Cell]
allCells = [(x, y) | x <- [0..8], y <- [0..8]]

-- 空いているセルのリストを返す
emptyCells :: Board -> [Cell]
emptyCells board = allCells \\ (map fst board)

-- 指定されたセルの同一縦横エリアで使われている数字のリストを返す
usedNumbers board cell = usedNumbersRow board cell ++ usedNumbersColumn board cell ++ usedNumbersArea board cell

-- 指定されたセルの同一横エリアで使われていない数字のリストを返す
availableNumbers board cell = [1..9] \\ (usedNumbers board cell)

-- 横
usedNumbersRow :: Board -> Cell -> [Int]
usedNumbersRow board cell = map snd $ filter (\c -> (snd (fst c)) == (snd cell)) board

-- 縦
usedNumbersColumn :: Board -> Cell -> [Int]
usedNumbersColumn board cell = map snd $ filter (\c -> (fst (fst c)) == (fst cell)) board

-- 同一エリア
usedNumbersArea board cell =
    let x = ((fst cell) `div` 3) * 3
        y = ((snd cell) `div` 3) * 3
    in  concat $ map (getNumberAt board) [(x + a, y + b) | a <- [0..2], b <- [0..2]]

-- 指定セルの数字を返す
-- 指定セルに数字があるなら長さ1のリスト
-- なければ空リスト
getNumberAt :: Board -> Cell -> [Int]
getNumberAt board cell = map snd $ filter (\c -> (fst c) == cell) board

-- 解く
solve :: Board -> [Board]
solve board | length board == 81 = [board]
solve board = concat [solve $ (c, n) : board
                     | let c = maximumBy (compare `on` length . usedNumbers board) $ emptyCells board,
                       n <- availableNumbers board c ]

-- 問題設定
problem :: Board
problem = [
        ((3, 0), 8),
        ((5, 0), 1),
        ((6, 1), 4),
        ((7, 1), 3),
        ((0, 2), 5),
        ((4, 3), 7),
        ((6, 3), 8),
        ((6, 4), 1),
        ((1, 5), 2),
        ((4, 5), 3),
        ((0, 6), 6),
        ((7, 6), 7),
        ((8, 6), 5),
        ((2, 7), 3),
        ((3, 7), 4),
        ((3, 8), 2),
        ((6, 8), 6)
    ]

-- Boardを表示用に整形する
format = map (map snd) . transpose . groupBy ((==) `on` (fst . fst)) . sort

main = do
    --print $ emptyCells problem
    --print $ usedNumbersRow problem (0,3)
    --print $ usedNumbersColumn problem (0,3)
    --print $ usedNumbersArea problem (0,8)
    --print $ getNumberAt problem (3, 0)
    mapM_ print $ format $ head $ solve problem
