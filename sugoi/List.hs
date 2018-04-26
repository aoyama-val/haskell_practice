data MyList a = Nil | Cons a (MyList a) deriving (Show)

myCar :: (Eq a) => MyList a -> a
myCar Nil = error "empty list"
myCar (Cons a b) = a

myCdr :: (Eq a) => MyList a -> MyList a
myCdr Nil = error "empty list"
myCdr (Cons a b) = b

myLength :: (Eq a) => MyList a -> Int
myLength Nil = 0
myLength (Cons a b) = 1 + (myLength b)

