module Problem26 (combinations) where

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n _ | n < 1 = error "Out of range"
combinations n []  = []
combinations 1 xs = map (\x -> [x]) xs
combinations n (x:xs) = (map (\y -> x:y) (combinations (n-1) xs)) ++ (combinations n xs)
