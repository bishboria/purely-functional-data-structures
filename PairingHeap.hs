data Heap a = E
            | T a [Heap a]
              deriving (Show, Eq)

empty :: Heap a
empty = E

isEmpty :: Heap a -> Bool
isEmpty E = True
isEmpty _ = False

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (T x []) h

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T x hs1) h2@(T y hs2)
    | x <= y    = T x (h2:hs1)
    | otherwise = T y (h1:hs2)

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs []         = E
mergePairs [h]        = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

findMin :: Heap a -> a
findMin E       = error "Empty heap"
findMin (T x _) = x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin E        = error "Empty heap"
deleteMin (T x hs) = mergePairs hs
