module Problem62 where

import NNPTrees

-- simplest solution
internal :: Tree a -> [a]
internal Empty = []
internal (Branch x Empty Empty) = []
internal (Branch x t1 t2) = x:(internal t1 ++ internal t2)

-- avoid the ++ operator for speed
internal2 :: Tree a -> [a]
internal2 t = ih t []
    where
        ih Empty xs = xs
        ih (Branch x Empty Empty) xs = xs
        ih (Branch x t1 t2) xs = (x:) $ ih t1 $ ih t2 xs
