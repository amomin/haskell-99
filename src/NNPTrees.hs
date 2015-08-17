module NNPTrees where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty
