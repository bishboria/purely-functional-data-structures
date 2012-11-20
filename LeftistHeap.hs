data Heap a = E
            | Heap Int a (Heap a) (Heap a)
              deriving (Show, Eq, Ord)

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge h1@(Heap _ x a1 b1) h2@(Heap _ y a2 b2)
    | x <= y    = makeT x a1 $ merge b1 h2
    | otherwise = makeT y a2 $ merge h1 b2
merge h E       = h
merge E h       = h

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (Heap 1 x E E) h

rank :: Heap a -> Int
rank E              = 0
rank (Heap r _ _ _) = r

makeT :: (Ord a) => a -> Heap a -> Heap a -> Heap a
makeT x a b
    | rank a >= rank b = Heap (rank b + 1) x a b
    | otherwise        = Heap (rank a + 1) x b a

empty :: Heap a
empty = E

isEmpty :: (Ord a) => Heap a -> Bool
isEmpty E = True
isEmpty _ = False

findMin :: (Ord a) => Heap a -> a
findMin (Heap _ n _ _) = n
findMin E              = error "Empty heap has no minimum value"

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin (Heap _ _ a b) = merge a b
