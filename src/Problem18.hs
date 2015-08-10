module Problem18 where

slice1 :: [a] -> Int -> Int -> [a]
slice1 lst a b 
    | a < 1     = error "Range"
    | b < a     = error "Range"
    | otherwise = (take (b - a + 1) (drop (a - 1) lst))
