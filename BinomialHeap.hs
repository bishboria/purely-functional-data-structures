data Tree a = Node Int a [Tree a]
              deriving (Show, Eq)

type Heap a = [Tree a]

empty = []

isEmpty ts = null ts

rank :: Tree a -> Int
rank (Node r _ _) = r

root :: Tree a -> a
root (Node _ n _) = n

link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
    | x1 <= x2  = Node (r+1) x1 (t2 : c1)
    | otherwise = Node (r+1) x2 (t1 : c2)

insert :: Ord a => a -> Heap a -> Heap a
insert x ts = insTree (Node 0 x []) ts

insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t ts@(t':ts')
    | rank t < rank t' = t : ts
    | otherwise        = insTree (link t t') ts'

merge :: Ord a => Heap a -> Heap a -> Heap a
merge t  [] = t
merge [] t  = t
merge ts1@(t1:ts1') ts2@(t2:ts2')
    | rank t1 < rank t2 = t1 : merge ts1' ts2
    | rank t2 < rank t1 = t2 : merge ts2' ts1
    | otherwise         = insTree (link t1 t2) (merge ts1' ts2')

findMin :: Ord a => Heap a -> a
findMin = root . fst . removeMinTree

deleteMin :: Ord a => Heap a -> Heap a
deleteMin ts = merge (reverse ts1) ts2
  where
    (Node _ x ts1, ts2) = removeMinTree ts

removeMinTree :: Ord a => Heap a -> (Tree a, Heap a)
removeMinTree [] = error "Can't remove anything from an empty heap"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) =
    let (t', ts') = removeMinTree ts
    in  if   root t < root t'
        then (t, ts)
        else (t', t:ts')
