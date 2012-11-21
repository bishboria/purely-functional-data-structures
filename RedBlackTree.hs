data Color  = Red
            | Black
              deriving (Show, Eq)

data Tree a = Empty
            | Tree Color (Tree a) a (Tree a)
              deriving (Show, Eq)

empty :: Tree a
empty = Empty

member :: Ord a => a -> Tree a -> Bool
member _ Empty  = False
member x (Tree _ l n r)
    | x < n     = member x l
    | x > n     = member x r
    | otherwise = True

insert :: Ord a => a -> Tree a -> Tree a
insert x t = Tree Black l n r
  where
    Tree _ l n r    = ins t
    ins Empty       = Tree Red Empty x Empty
    ins s@(Tree color a y b)
        | x < y     = balance color (ins a) y b
        | x > y     = balance color a y (ins b)
        | otherwise = s

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance Black (Tree Red (Tree Red a x b) y c) z d = thing a x b y c z d
balance Black (Tree Red a x (Tree Red b y c)) z d = thing a x b y c z d
balance Black a x (Tree Red (Tree Red b y c) z d) = thing a x b y c z d
balance Black a x (Tree Red b y (Tree Red c z d)) = thing a x b y c z d
balance color l x r                               = Tree color l x r

-- Factoring out the repeated code. Can't think of a name
thing a x b y c z d = Tree Red (Tree Black a x b) y (Tree Black c z d)
