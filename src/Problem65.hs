module Problem65 where

import NNPTrees

-- almost same as solution
-- added the xoffset fix after seeing solution (using left depth idea)
layout :: Tree a -> Tree (a,(Int,Int))
layout Empty = Empty
layout t = lAux t (1-2^((theight-tLHeight))) $ theight
    where
        height Empty = 0
        height (Branch _ l r) = 1 + max (height l) (height r)
        lHeight Empty = 0
        lHeight (Branch _ l _) = 1 + (lHeight l)
        theight = height t
        tLHeight = lHeight t
        lAux Empty xoff ht = Empty
        lAux (Branch x l r) xoff ht
            = Branch (x,(xoff + 2^(ht-1), theight - ht + 1))
                (lAux l xoff (ht-1))
                (lAux r (xoff + 2^(ht-1)) (ht-1))
