-- プログラミングHaskell
-- 第11章
-- 本の通りのプログラム

-- 補助関数
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
                where
                    yss = subs xs

-- 補助関数
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- 補助関数
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- 補助関数
choices :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

-- 補助関数
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls, rs) <- split xs]

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = "(" ++ (show l) ++ (show o) ++ (show r) ++ ")"

-- 式を評価し、数のリストを返す
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- 式の中で使われている数のリストを返す
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- 解かどうか判定する
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- 全ての演算子のリストを返す
ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- 2つの式を組み合わせて作れる全ての式を返す
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

-- 数字のリストから組み合わせで作れる全ての式を返す
-- （中には 3 / 4 など無効な式もある）
-- 数字が現れる順序は固定
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

-- 解く
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]


main = do
    --print $ solution (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)))  [1,3,7,10,25,50] 765
    print $ head $ solutions [1,3,7,10,25,50] 765
