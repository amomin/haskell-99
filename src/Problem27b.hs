module Problem27b (group1) where

group1 :: [Int] -> [a] -> [[[a]]]
group1 [] _ = error "must pass ints"
--group 1 ns xs | (sum ns) \= (length xs) = error "must sum"
group1 ms ys = map tail $ group1h ms ys
    where
        group1h [n] x = group2 n x
        group1h (n:ns) xs = concatMap (\(hd:tl) -> map (\[z1,z2] -> z1:z2:tl) $ group2 n hd) $ group1h ns xs
        group2 :: Int -> [a] -> [[[a]]]
        group2 0 xs = [[xs, []]]
        group2 n [] = []
        group2 n xs@(hd:tl) = (map (\[a,b] -> [a,hd:b]) (group2 (n-1) tl)) 
                                ++ (map (\[a,b] -> [hd:a,b]) (group2 (n) tl))

-- from solutions
-- this suggestion for group2 (they call it - naturally - combinations)
-- including this solution because it uses comprehensions
combinations :: Int -> [a] -> [([a],[a])]
combinations 0 xs = [(xs,[])]
combinations _ [] = []
combinations n (x:xs) = l1 ++ l2
    where
        l1 = [ (x:x1s,x2s) | (x1s,x2s) <- combinations n xs ]
        l2 = [ (x1s,x:x2s) | (x1s,x2s) <- combinations (n-1) xs ]
