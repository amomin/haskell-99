module Problem03 where

elementAt :: Integer -> [a] -> a
elementAt n xs
    | n < 1 = error "Must provide a non-negative integer"
    | (n > 1 && (null xs)) = error "Not enough elements in list"
    | n == 1 = head xs
    | otherwise = elementAt (n-1) (tail xs)

-- Attempt to define in terms of existing operations only
elementAt' :: Integer -> [a] -> a
elementAt' n xs = head (foldl (id (const . tail)) xs [1..(n-1)])
