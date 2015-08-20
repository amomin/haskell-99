module NNPTrees where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

dfs :: Tree a -> [a]
dfs t = helper t []
    where
        helper Empty xs = xs
        helper (Branch x l r) xs = x:(helper l $ helper r xs)

bfs :: Tree a -> [a]
bfs t = bfsForest [t]

bfsForest :: [Tree a] -> [a]
bfsForest [] = []
bfsForest ts = (heads ts) ++ bfsForest (concatMap split ts)
heads [] = []
heads (Empty:ts) = heads ts
heads ((Branch x l r):ts) = x:(heads ts)
split Empty = []
split (Branch _ l r) = [l, r]

isomorphic :: Tree a -> Tree b -> Bool
isomorphic Empty Empty = True
isomorphic _ Empty = False
isomorphic Empty _ = False
isomorphic (Branch _ x1 y1) (Branch _ x2 y2) = and [isomorphic x1 x2, isomorphic y1 y2]
