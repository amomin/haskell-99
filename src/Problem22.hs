module Problem22 where

range1 :: Int -> Int -> [Int]
range1 a b 
    | a > b = [a,(a-1)..b]
    | otherwise = [a..b]

-- as pointed out in solutinos, this is basically enumFromTo
-- (add a case to deal with decreasing lists)
range2 :: Int -> Int -> [Int]
range2 = enumFromTo

-- also as pointed out in solutinos
range3 :: (Enum a, Ord a) => a -> a -> [a]
range3 a b | (a == b) = [a]
range3 a b = a : range3 ((if a > b then pred else succ) a ) b
