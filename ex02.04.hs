{- Combine the ideas of the previous two exercises to obtain a version of
 - insert that performs no unnecesary copying and uses no more than d+1
 - comparisons.
 -}

data Tree a = Empty
            | T (Tree a) a (Tree a)
              deriving (Show, Eq)

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty       = T Empty x Empty
insert x t@(T _ n _) = _insert n t id
  where
    _insert last Empty f
        | x == last = t
        | otherwise = f (T Empty x Empty)
    _insert last (T l n r) f
        | x < n     = _insert last l $ f . (\lt -> T lt n r)
        | otherwise = _insert n r $ f . (\rt -> T l n rt)
