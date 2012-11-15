module Stack where

import Prelude hiding ( head, tail )

class Stack s where
    empty    :: s a
    isEmpty  :: s a -> Bool
    cons     :: a -> s a -> s a
    head     :: s a -> a
    tail     :: s a -> s a
    (.++.)   :: s a -> s a -> s a
    update   :: s a -> Int -> a -> s a
    suffixes :: s a -> s (s a)

instance Stack [] where
    empty       = []

    isEmpty []  = True
    isEmpty _   = False

    cons        = (:)

    head (x:xs) = x
    head _      = undefined

    tail (x:xs) = xs
    tail _      = undefined

    (.++.)      = (++)

    update []     _ _ = undefined
    update (x:xs) 0 y = cons y xs
    update (x:xs) i y = cons x $ update xs (i - 1) y

    suffixes [] = [[]]
    suffixes xs = cons xs (suffixes $ tail xs)


data CustomStack a = Nil
                   | Cons a (CustomStack a)
                     deriving (Show)

instance Stack CustomStack where
    empty         = Nil

    isEmpty Nil   = True
    isEmpty _     = False

    cons x s      = Cons x s

    head (Cons x _) = x
    head _          = undefined

    tail (Cons _ s) = s
    tail _          = undefined

    xs .++. ys      = if   isEmpty xs
                      then ys
                      else cons (head xs) (tail xs .++. ys)

    update Nil         _ _ = undefined
    update (Cons x xs) 0 y = cons y xs
    update (Cons x xs) i y = cons x $ update xs (i-1) y

    suffixes Nil    = Cons Nil Nil
    suffixes xs     = cons xs (suffixes $ tail xs)
