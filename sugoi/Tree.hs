data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton:: a -> Tree a
singleton a = Node a EmptyTree EmptyTree

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree a EmptyTree = singleton a
insertTree a (Node b left right)
    | a == b    = Node b left right
    | a < b     = Node b (insertTree a left) right
    | a > b     = Node b left (insertTree a right)

treeElem x EmptyTree = False
treeElem x tree@(Node a left right)
    | x == a    = True
    | x < a     = treeElem x left
    | otherwise = treeElem x right


main = do
    let mylist1 = Nil
    let mylist2 = Cons 5 mylist1
    let mylist3 = Cons 6 mylist2
    print $ mylist2
    print $ mylist3
    print $ myCar mylist3
    print $ myCdr mylist3
    print $ myCar $ myCdr mylist3
    print $ myLength mylist3
    let xs = [8, 6, 4, 1, 7, 3, 5]
    let tree = foldr insertTree EmptyTree xs
    print $ treeElem 1 tree
    print $ treeElem 3 tree
    print $ treeElem 9 tree
    print $ treeElem (-1) tree
    --print $ singleton 5
