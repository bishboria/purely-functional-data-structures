type Queue a = ([a],[a])

empty = ([],[])

isEmpty :: Queue a -> Bool
isEmpty ([], _) = True

head :: Queue a -> a
head ([], _) = error "Can't take the head of an empty queue"
head (x:f,r) = x

tail :: Queue a -> Queue a
tail ([],_)  = error "Can't get the tail of an empty queue"
tail (_:f,r) = checkf (f,r)

snoc :: Queue a -> a -> Queue a
snoc (f,r) x = checkf (f,x:r)

checkf :: Queue a -> Queue a
checkf ([], r) = (reverse r, [])
checkf q       = q
