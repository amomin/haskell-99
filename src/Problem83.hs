module Problem83 where

import Data.List (partition)
import NNPGraphs

-- Solution is pretty slow
-- This problem probably scales very badly (factorially on average maybe?)
-- but this solution is already bad on the complete graph k(6) (containing only 1276 subtrees)

spantree :: (Eq a) => Graph a -> [[(a,a)]]
spantree = spantree3

is_connected :: (Eq a) => Graph a -> Bool
is_connected g = 0 /= (length $ spantree g)

is_tree :: (Eq a) => Graph a -> Bool
is_tree g = 1 == (length $ spantree g)

adjTo :: (Eq a) => a -> [(a,a)] -> [(a,a)]
adjTo v edgs = filter (\(x,y) -> x == v || y == v) edgs

vertices :: [(a,a)] -> [a]
vertices edgs = (map (\e -> fst e) edgs) ++ (map (\e -> snd e) edgs)

edges :: Graph a -> [(a,a)]
edges (Graph vs es) = es
verts :: Graph a -> [a]
verts (Graph vs es) = vs

-- first solution - should produce all (non-directed) trees but with 
-- not-very-natural redundancies
spantree1 :: (Eq a) => Graph a -> [[(a,a)]]
spantree1 (Graph vs es) = helper 1 (es ++ (map (\(x,y) -> (y,x)) es)) $ map (\x -> [x]) $ adjTo strt es
    where
        strt = head vs
        n = length vs
        helper :: (Eq a) => Int -> [(a,a)]  -> [[(a,a)]] -> [[(a,a)]]
        helper len edgs candidates
            | len == (n-1)  = candidates
            | otherwise = helper (len+1) edgs newcandidates
            where
                newcandidates = 
                    [ e:candidate 
                    | candidate <- candidates, 
                      e <- edgs, 
                      (not (elem (fst e) $ vertices candidate)) && (snd e) == (fst $ head candidate)
                    ] 
                    ++
                    [ candidate ++ [e]
                    | candidate <- candidates, 
                      e <- edgs, 
                      (not (elem (snd e) $ vertices candidate)) && (fst e) == (snd $ head $ reverse candidate)
                    ]

filterEquivalent :: (Eq a) => [Graph a] -> [Graph a]
filterEquivalent [] = []
filterEquivalent (g:gs) = (g:(filterEquivalent (filter (\x -> not $ equivalent g x) gs)))
    where
        equivalent :: (Eq a) => Graph a -> Graph a -> Bool
        equivalent g@(Graph gv ge) h@(Graph hv he) = 
            --(0 == (length $ filter (\x -> not $ elem x gv) hv))
            (0 == (length $ filter (\x -> not $ elem x gv) hv))
            && (0 == (length $ filter (\x -> not $ elem x hv) gv))
            && (0 == (length $ filter (\(x,y) -> (not $ elem (x,y) ge) && (not $ elem (y,x) ge)) he))
            && (0 == (length $ filter (\(x,y) -> (not $ elem (x,y) he) && (not $ elem (y,x) he)) ge))

-- Provide the trees found as graphs
spantree2 :: (Eq a) => Graph a -> [Graph a]
spantree2 g@(Graph vs es) = helper g 1 $ map (\(x,y) -> (Graph [x,y] [(x,y)])) es
    where
        n = length vs
        helper :: (Eq a) => Graph a -> Int -> [Graph a] -> [Graph a]
        helper g len candidates
            | len == (n-1) = candidates
            | otherwise = helper g (len+1) newcandidates
            where
                edgs = edges g
                newcandidates = [ (Graph ((if (elem v1 vs1) then v2 else v1):vs1) (e:es1))
                                | (Graph vs1 es1) <- candidates,
                                  e@(v1,v2) <- edgs,
                                  ((not (elem v1 vs1)) && (elem v2 vs1)) || ((not (elem v2 vs1)) && (elem v1 vs1))
                                ]

-- As above but this one will filter out equivalent branches at each iteration
spantree3 :: (Eq a) => Graph a -> [Graph a]
spantree3 g@(Graph vs es) = helper g 1 $ map (\(x,y) -> (Graph [x,y] [(x,y)])) es
    where
        n = length vs
        helper :: (Eq a) => Graph a -> Int -> [Graph a] -> [Graph a]
        helper g len candidates
            | len == (n-1) = candidates
            | otherwise = helper g (len+1) $ filterEquivalent newcandidates
            where
                edgs = edges g
                newcandidates = [ (Graph ((if (elem v1 vs1) then v2 else v1):vs1) (e:es1))
                                | (Graph vs1 es1) <- candidates,
                                  e@(v1,v2) <- edgs,
                                  ((not (elem v1 vs1)) && (elem v2 vs1)) || ((not (elem v2 vs1)) && (elem v1 vs1))
                                ]
