module Problem84 where

import NNPGraphs

prim :: (Eq a) => WeightedGraph a -> (WeightedGraph a, Int)
prim g@(WeightedGraph vs es) = helper (WeightedGraph vs (helpsort es)) 0 (WeightedGraph [(head vs)] []) 0
    where
        n = length vs
        helper g@(WeightedGraph vs1 es1) len premst@(WeightedGraph pvs pes) sum
            | len == (n-1) = (premst, sum)
            | otherwise = 
                helper (WeightedGraph vs remainingedges) (len+1) (WeightedGraph (v:pvs) (e:pes)) (sum+w)
                where
                    -- take the least weighted edge connecting current premst to unconnected vertex
                    e@(v1,v2,w) = head $ dropWhile 
                                            (\(x,y,_) -> not 
                                            (((elem x $ pvs) && (not $ elem y $ pvs))
                                            || ((elem y $ pvs) && (not $ elem x $ pvs)))
                                            ) es1
                    v = if (not $ elem v1 pvs) then v1 else v2
                    remainingedges = filter (/= e) es1
        helpsort :: [(a,a,Int)] -> [(a,a,Int)]
        helpsort [] = []
        helpsort (e@(_,_,n):es) = (helpsort $ filter (\(_,_,m) -> (m < n)) es)
                                ++ (e:(filter (\(_,_,m) -> (m == n)) es))
                                ++  (helpsort $ filter (\(_,_,m) -> (m > n)) es)
