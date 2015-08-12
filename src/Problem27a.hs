module Problem27a (group3a, group3b) where

group3a :: [a] -> [[[a]]]
group3a xs | (length xs) /= 9 = error "List must have nine elements by problem statement"
group3a x = 
    let
        group2 :: Int -> [a] -> [([a],[a])]
        group2 0 xs = [([], xs)]
        group2 n [] = []
        group2 n xs@(hd:tl) = (map (\(a,b) -> (hd:a,b)) (group2 (n-1) tl)) 
                                ++ (map (\(a,b) -> (a,hd:b)) (group2 (n) tl))
        h :: [a] -> [([a],[a],[a])]
        h xs = concatMap (\(x,y) -> (map (\(z1,z2) -> (x,z1,z2)) (group2 3 y) ) ) (group2 2 xs)
    in map (\(x1,x2,x3) -> [x1,x2,x3]) (h x)


group3b :: [a] -> [[[a]]]
group3b xs = concatMap (\[x,y] -> map (\[z1,z2] -> [x,z1,z2]) $ group2 3 y) $ group2 4 xs
    where
        group2 :: Int -> [a] -> [[[a]]]
        group2 0 xs = [[[], xs]]
        group2 n [] = []
        group2 n xs@(hd:tl) = (map (\[a,b] -> [hd:a,b]) (group2 (n-1) tl)) 
                                ++ (map (\[a,b] -> [a,hd:b]) (group2 (n) tl))

-- helpers along the way
--f :: [a] -> [([a],[([a],[a])])]
--f xs = map (\(x,y) -> (x, group2 4 y)) (group2 3 xs)
--g :: [a] -> [[([a],[a],[a])]]
--g xs = map (\(x,y) -> (map (\(z1,z2) -> (x,z1,z2)) (group2 3 y) ) ) (group2 2 xs)
--h :: [a] -> [([a],[a],[a])]
--h xs = concatMap (\(x,y) -> (map (\(z1,z2) -> (x,z1,z2)) (group2 3 y) ) ) (group2 2 xs)
--group2 :: Int -> [a] -> [([a],[a])]
--group2 0 xs = [([], xs)]
--group2 n [] = []
--group2 n xs@(hd:tl) = (map (\(a,b) -> (hd:a,b)) (group2 (n-1) tl)) 
                        -- ++ (map (\(a,b) -> (a,hd:b)) (group2 (n) tl))
