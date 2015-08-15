module Problem41 where

import Problem31(primesUpTo)
import Problem40(goldbach)

-- just use problem to problem 40 and map it
goldbachList :: Int -> Int -> [(Int,Int)]
--goldbachList a b = map goldbach $ [a,(a+2)..b]
--goldbachList a b = map goldbach $ filter (\x -> and [(x > 3), even x]) [a..b]
goldbachList a b = map goldbach
    $ filter (even)
    $ dropWhile (< 4) [a..b]

-- use above solution and filter
-- could implement a faster solution by simply
-- reimplementing goldbach so as to stop seeking solutions 
-- when a prime exceeds the threshold
goldbachList' :: Int -> Int -> Int -> [(Int,Int)]
goldbachList' a b m = filter (\(x1,x2) -> x1 > m) $ goldbachList a b
