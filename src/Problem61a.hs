module Problem61a where

import NNPTrees

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ t1 t2) = leaves t1 ++ leaves t2

-- avoid the ++ operator (from solutions)
leaves2 :: Tree a -> [a]
leaves2 t = lh t []
    where
        lh Empty xs = xs
        lh (Branch x Empty Empty) xs = x:xs
        lh (Branch _ t1 t2) xs = lh t1 $ lh t2 xs
