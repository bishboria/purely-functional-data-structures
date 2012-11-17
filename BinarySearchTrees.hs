data Tree a = Empty
            | T (Tree a) a (Tree a)
              deriving (Show)

member :: Ord a => a -> Tree a -> Bool
member _ Empty  = False
member x (T l n r)
    | x < n     = member x l
    | x > n     = member x r
    | otherwise = True

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty  = T Empty x Empty
insert x t@(T l n r)
    | x < n     = T (insert x l) n r
    | x > n     = T l n (insert x r)
    | otherwise = t
