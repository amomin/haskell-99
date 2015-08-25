module Problem80 where

import NNPGraphs

adjToGraph :: (Eq a) => Adj a -> Graph a
graphToAdj :: (Eq a) => Graph a -> Adj a

graphToAdj (Graph vertices edges) = Adj $ helper vertices edges
    where
        helper :: Eq a => [a] -> [(a,a)] -> [(a,[a])]
        helper [] es = []
        helper vs@(v:vst) edges = ((v, filterEdges v edges):) (helper vst edges)
        filterEdges vertex edgeSet 
            = map (\(x,y) -> if y == vertex then x else y) 
                $ filter (\x -> (fst x) == vertex || (snd x) == vertex) edgeSet

adjToGraph adjList = let (nodes,edges) = helper adjList in Graph nodes edges
    where
        helper :: (Eq a) => Adj a -> ([a],[(a,a)])
        helper (Adj []) = ([], [])
        helper adjList@(Adj ((x,adjs):tl))
            = (x:nodes1, (map (\adj -> (x,adj)) adjs)++edges1)
            where
                (nodes1,edges1) = helper (Adj (filterVertex x tl))
                filterVertex v edges = map (\(x,vs) -> (x, filter (\y -> y /= v) vs)) edges

-- forgot to the friendly case
graphToFriendly :: (Eq a) => Graph a -> Friendly a
graphToFriendly (Graph xs ys) = Friendly (ys ++ unconnected xs ys)
    where
        -- not efficient but works
        zipUp es = (map fst es) ++ (map snd es)
        unconnected :: (Eq a) => [a] -> [(a,a)] -> [(a,a)]
        unconnected vs es = map (\x -> (x,x)) $ filter (\x -> (not $ elem x (zipUp es))) vs

adjToFriendly :: (Eq a) => Adj a -> Friendly a
adjToFriendly = graphToFriendly . adjToGraph

friendlyToGraph :: (Eq a) => Friendly a -> Graph a
friendlyToGraph (Friendly edges) = Graph (vertexSet edges) (edgeSet edges)
    where
        vertexSet es = uniqueVals $ ((map fst es) ++ (map snd es))
        edgeSet es = filter (\x -> (fst x) /= (snd x)) es
        uniqueVals :: (Eq a) => [a] -> [a]
        uniqueVals [] = []
        uniqueVals (x:xs) = if (not $ elem x xs) then (x:(uniqueVals xs)) else (uniqueVals xs)

friendlyToAdj :: (Eq a) => Friendly a -> Adj a
friendlyToAdj = graphToAdj . friendlyToGraph
