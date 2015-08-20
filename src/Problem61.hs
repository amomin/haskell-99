module Problem61 where

import NNPTrees

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ t1 t2) = countLeaves t1 + countLeaves t2
