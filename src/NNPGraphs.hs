module NNPGraphs where

data Graph a = Graph [a] [(a,a)] deriving (Eq, Show)
data Adj a = Adj [(a,[a])] deriving (Eq, Show)
data Friendly a = Friendly [(a,a)] deriving (Eq, Show)

-- exactly the same representation!  But order matters
data DiGraph a = DiGraph [a] [(a,a)] deriving (Eq, Show)

data WeightedGraph a = WeightedGraph [a] [(a,a,Int)] deriving (Eq, Show)

-- exactly the same representation!  But order matters
data WeightedDiGraph a = WeightedDiGraph [a] [(a,a,Int)] deriving (Eq, Show)

kgraph :: Int -> Graph Int
kgraph n = Graph [1..n] [(x,y) | x <- [1..(n-1)], y<-[(x+1)..n]]

--isomorphic :: (Eq a) => Graph a -> Graph a -> Bool

