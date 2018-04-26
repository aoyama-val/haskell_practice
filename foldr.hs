-- http://qiita.com/7shi/items/1345bf32003faff435cb

map' f [] = []
map' f (x:xs) = (f x) : (map' f xs)

filter' f [] = []
filter' f (x:xs) = 
    if (f x) then
        x : (filter' f xs)
    else
        (filter' f xs)

flip' f a b = f b a

foldr' f init [] = init
foldr' f init (x:xs) = f init (foldr' f x xs)

foldl' f init [] = init
foldl' f init (x:xs) = (foldl' f (f init x) xs)

main = do
    print $ map' (* 2) [1..5]
    print $ filter' (< 5) [1..9]
    print $ flip' map' [1..5] (* 2)
    print $ foldl' (+) 0 [1..100]
    print $ foldl' (-) 0 [1..5]
    print $ foldr' (-) 0 [1..5]
