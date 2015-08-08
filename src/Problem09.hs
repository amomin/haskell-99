module Problem09 where

pack1 :: (Eq a) => [a] -> [[a]]
pack1 [] = []
--pack1 (x:xs) = helper x [] x:xs
pack1 (x:xs) = reverse (helper x [[x]] xs)
    where
        helper _ sofar [] = sofar
        helper _ [] (el:els) = helper el [[el]] els
        helper curr sofar (el:els) 
            | el == curr = helper el ((el:(head sofar)):(tail sofar)) els 
            | otherwise =  helper el ([el]:sofar) els 

-- From solutions

-- using 'span'
-- demonstrates use of the 'span' library function
-- which splits a list into two pieces according to
-- a boolean criterion on its elements: the first piece
-- is the set of elements up to (not including) the first 
-- failure, and the second piece is the remaining set of elements
pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) = 
    let (first, second) = span (==x) xs
        in (x:first) : pack2 second

-- Using takeWhile and dropWhile (very useful functions)
pack3 :: (Eq a) => [a] -> [[a]]
pack3 [] = []
pack3 (x:xs) = (x:(takeWhile (==x) xs)) : pack3 (dropWhile (==x) xs)

pack :: (Eq a) => [a] -> [[a]]
pack = pack2
