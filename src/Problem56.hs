module Problem56 where

import NNPTrees

symmetric ::  Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ x y) = mirror x y

-- simplified pattern after seeing solution
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ x1 y1) (Branch _ x2 y2) = and [mirror x1 y2, mirror y1 x2]
mirror _ _ = False

--isomorphic :: Tree a -> Tree a -> Bool
--isomorphic Empty Empty = True
--isomorphic _ Empty = False
--isomorphic Empty _ = False
--isomorphic (Branch _ x1 y1) (Branch _ x2 y2) = and [isomorphic x1 x2, isomorphic y1 y2]
