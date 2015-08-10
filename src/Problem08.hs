module Problem08 where

import Data.List

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = helper x [x] xs
    where
        helper :: (Eq a) => a -> [a] -> [a] -> [a]
        helper el sofar [] = reverse sofar
        helper el sofar (y:ys)
            | y == el   = helper y sofar ys
            | otherwise = helper y (y:sofar) ys

-- solutions below based on provided solutions
-- Demonstrates the pattern operator '@'
-- see e.g. http://stackoverflow.com/questions/1153465/..
--  ../what-does-the-symbol-mean-in-reference-to-lists-in-haskell
compress1 :: (Eq a) => [a] -> [a]
compress1 (x:xs@(y:_))
    | x == y = compress xs
    | otherwise = x : compress xs

-- uses the 'group' operator from Data.List
compress2 :: (Eq a) => [a] -> [a]
compress2 = map head . group
--Equivalently
--compress2 = map head $ group
