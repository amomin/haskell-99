module Problem04 where

myLength :: [a] -> Int
myLength xs = length xs

-- A recursive definition (no library functions)
myLength' :: [a] -> Int
myLength' [] = 0
myLength' xs  = (1 + myLength' (tail xs))
