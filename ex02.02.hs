{- Change member function to use at most d+1 comparisons (rather than 2d
 - comparisons) where d is the depth of the tree.
 -}

data Tree a = Empty
            | T (Tree a) a (Tree a)
              deriving (Show)

-- original 2d comparisons
member :: Ord a => a -> Tree a -> Bool
member _ Empty  = False
member x (T l n r)
    | x < n     = member x l
    | x > n     = member x r
    | otherwise = True

-- d+1 comparisons
member' :: Ord a => a -> Tree a -> Bool
member' x t@(T _ n _) = _member' n t
  where
    _member' last Empty
        | x == last = True
        | otherwise = False
    _member' last (T l n r)
        | x < n     = _member' last l
        | otherwise = _member' n r
