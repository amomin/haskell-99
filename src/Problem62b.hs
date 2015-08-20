module Problem62 where

import NNPTrees

-- simplest solution
atLevel :: Tree a -> Int -> [a]
atLevel _ n | n < 1 = error "tree level must be at least 1"
atLevel Empty _ = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch x l r) n = (atLevel l (n-1)) ++ (atLevel r (n-1))

-- avoid the ++ operator for speed
atLevel1 :: Tree a -> Int -> [a]
atLevel1 t n = alh t n []
    where
        alh Empty _ xs = xs
        alh (Branch x t1 t2) 1 xs = x:xs
        alh (Branch x t1 t2) n xs = alh t1 (n-1) $ alh t2 (n-1) xs
