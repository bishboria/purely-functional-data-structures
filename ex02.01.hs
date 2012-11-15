{- Write a function suffixes of type [a] -> [[a]] that takes a list xs and
   returns a list of all the suffixes of xs in decreasing order of length. For
   example:

       suffixes [1,2,3,4] = [[1,2,3,4],[2,3,4],[3,4],[4],[]]

   Show that the resulting list of suffixes can be generated in O(n) time and
   represented in O(n) space.
-}

-- See Stack.hs for full implementation using 2 data structures
suffixes [] = [[]]
suffixes xs = xs : suffixes (tail xs)

-- tail is O(1)
-- (:)  is O(1)
-- suffixes calls each of the elements of xs in turn so O(n)

-- xs ->          [1|-]->[2|-]->[3|-]->[4|\]
--                 |      |      |      |
-- suffixes xs -> [||-]->[||-]->[||-]->[||-]->[\|\]
--
-- O(2n+1) == O(n) space to represent
