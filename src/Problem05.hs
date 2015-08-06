module Problem05 where

-- trivial using library function
myReverse :: [a] -> [a]
myReverse = reverse

-- an actual solution - not a good solution but "obvious"
myReverse1 :: [a] -> [a]
myReverse1 [] = []
myReverse1 [x] = [x]
myReverse1 (x:xs) = (myReverse1 xs) ++ [x]

-- using an accumulator
myReverse2 :: [a] -> [a]
myReverse2 [] = []
myReverse2 [x] = [x]
myReverse2 (x:xs) = helper [x] xs
   where 
        helper sofar []  = sofar
        helper sofar (x:xs) = helper (x:sofar) xs

-- using folds
myReverse3 :: [a] -> [a]
myReverse3 = foldl (\x y -> y:x) []

-- from the solutions page, apparently the "standard prelude" definition
-- which (I think) is identical to the above (myReverse3) but (nicely) realizing
-- the lambda expression as a composition of flip and (:)
myReverse4 :: [a] -> [a]
myReverse4 = foldl (flip (:)) []
