module Problem81 where

import NNPGraphs

-- not a very efficient method but (hopefully) does the job
-- needs a good test set
paths1 :: (Eq a) => Graph a -> a -> a -> [[a]]
paths1 g x y = reverse $ helper g x y [x]
    where
        helper g end y currPath | end == y = [currPath]
        helper g v y currPath = concat
            $ map (\n -> (helper g n y (n:currPath)) )
            $ filter (\n -> not $ elem n currPath)
            $ neighbors g v
        
neighbors (Graph vs es) x = uniqueVals 
    $ map (\(y1,y2) -> if y1 == x then y2 else y1) 
    $ filter (\(y1,y2) -> y1 == x || y2 == x) es
    where
        uniqueVals :: (Eq a) => [a] -> [a]
        uniqueVals [] = []
        uniqueVals (x:xs) = if (not $ elem x xs) then (x:(uniqueVals xs)) else (uniqueVals xs)
