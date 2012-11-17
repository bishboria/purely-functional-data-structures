data Tree a = Empty
            | T (Tree a) a (Tree a)
              deriving (Show, Eq)

-- copies entire path even when element to insert already present
insert :: Ord a => a -> Tree a -> Tree a
insert x Empty  = T Empty x Empty
insert x t@(T l n r)
    | x < n     = T (insert x l) n r
    | x > n     = T l n (insert x r)
    | otherwise = t

-- build up a continuation unless element already exists.
insert' :: Ord a => a -> Tree a -> Tree a
insert' x t =
    _insert' t id
  where
    _insert' Empty f = f (T Empty x Empty)
    _insert' (T l n r) f
            | x < n     = _insert' l $ f . (\lt -> T lt n r)
            | x > n     = _insert' r $ f . (\rt -> T l n rt)
            | otherwise = t

tree = insert 5 $ insert 7 $ insert 6 $
       insert 3 $ insert 1 $ insert 2 $
       insert 4 Empty

tree2 = insert' 5 $ insert' 7 $ insert' 6 $
        insert' 3 $ insert' 1 $ insert' 2 $
        insert' 4 Empty

equalTrees = tree == tree2
