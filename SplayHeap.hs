data Tree a = E
            | T (Tree a) a (Tree a)
              deriving (Show, Eq)

empty :: Tree a
empty = E

isEmpty :: Tree a -> Bool
isEmpty E = True
isEmpty _ = False

partition :: Ord a => a -> Tree a -> (Tree a, Tree a)
partition _ E = (E,E)
partition pivot t@(T a x b)
    | x <= pivot =
        case b of
          E           -> (t, E)
          (T b1 y b2) -> if   y <= pivot
                         then let (small, big) = partition pivot b2
                              in  (T (T a x b1) y small, big)
                         else let (small, big) = partition pivot b1
                              in  (T a x small, T big y b2)
    | otherwise  =
        case a of
          E -> (E, t)
          (T a1 y a2) -> if   y <= pivot
                         then let (small, big) = partition pivot a2
                              in  (T a1 y small, T big x b)
                         else let (small, big) = partition pivot a1
                              in  (small, T big y (T a2 x b))

insert :: Ord a => a -> Tree a -> Tree a
insert x t = T a x b
  where
    (a, b) = partition x t

merge :: Ord a => Tree a -> Tree a -> Tree a
merge E t = t
merge (T a x b) t = T (merge ta a) x (merge tb b)
  where
    (ta, tb) = partition x t

findMin :: Tree a -> a
findMin E         = error "Empty heap"
findMin (T E x _) = x
findMin (T a _ _) = findMin a

deleteMin :: Tree a -> Tree a
deleteMin E = error "Empty heap"
deleteMin (T E _ b)         = b
deleteMin (T (T E _ b) y c) = T b y c
deleteMin (T (T a x b) y c) = T (deleteMin a) x (T b y c)
