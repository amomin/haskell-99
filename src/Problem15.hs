module Problem15 where

-- somewhat slow bc of the ++ operator, if n is large
repli1 :: [a] -> Int -> [a]
repli1 [] _ = []
repli1 (x:xs) n = (replicate n x) ++ repli1 xs n

-- from solutions
-- using concatMap
repli2 :: [a] -> Int -> [a]
repli2 xs n = concatMap (replicate n) xs
