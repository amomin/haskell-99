module NNPGraphs where

data Graph a = Graph [a] [(a,a)] deriving (Eq, Show)
data Adj a = Adj [(a,[a])] deriving (Eq, Show)
data Friendly a = Friendly [(a,a)] deriving (Eq, Show)
