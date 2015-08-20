module Problem69 where

import NNPTrees

tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch x l r) = (x:) (tree2ds l) ++ (tree2ds r)

-- after problem 67 this pattern is pretty straightforward to guess
-- sample solution is pretty much identical (had a redundant case
-- deleted after peeking at solution)
ds2tree :: String -> Tree Char
ds2tree str = fst $ hlp str
    where
        hlp "." = (Empty, "")
        hlp (y:tl) | y== '.' = (Empty, tl)
        hlp (x:tl) = (Branch x l r, rst2)
            where
                (l,rst1) = hlp tl
                (r,rst2) = hlp rst1
