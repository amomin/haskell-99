module Problem04 where

myLength :: [a] -> Int
myLength xs = length xs

-- A recursive definition (no library functions)
myLength' :: [a] -> Int
myLength' [] = 0
myLength' xs  = (1 + myLength' (tail xs))

-- Solutions below written after looking at solutions on website

-- as above but use pattern matching instead of tail
myLength'' :: [a] -> Int
myLength'' [] = 0
myLength'' (_:xs)  = (1 + myLength'' xs)

-- map to 1 and sum
myLength''' :: [a] -> Int
myLength''' xs = sum $ map (\x -> 1) xs

-- using fold
myLength'''' :: [a] -> Int
myLength'''' xs = foldl (\x y -> x + 1) 0 xs
-- Even better
--myLength'''' xs = foldl (\x _ -> x + 1) 0 xs
-- And even better
--myLength'''' = foldl (\x _ -> x + 1) 0

