module Problem55 where

import NNPTrees

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [Branch 'x' Empty Empty]
cbalTree n 
    | odd n = map (\(x,y) -> Branch 'x' x y) [(x,y) | x <- cbalTree n1, y <- cbalTree n2]
    | otherwise = map (\(x,y) -> Branch 'x' x y) [(x,y) | x <- cbalTree n1, y <- cbalTree n2]
                ++ map (\(x,y) -> Branch 'x' x y) [(y,x) | x <- cbalTree n1, y <- cbalTree n2]
    where
        n1 = (quot n 2)
        n2 = if (even n) then n1 - 1 else (quot n 2)

-- more or less from solutions - similar but using list comprehension instead, easier to read
cbalTree2 :: Int -> [Tree Char]
cbalTree2 0 = [Empty]
cbalTree2 1 = [Branch 'x' Empty Empty]
cbalTree2 n 
    | odd n = [Branch 'x' x y | x <- cbalTree2 n1, y <- cbalTree2 n2]
    | otherwise = concat [[Branch 'x' x y, Branch 'x' y x] | x <- cbalTree2 n1, y <- cbalTree2 n2]
                
    where
        n1 = (quot n 2)
        n2 = if (even n) then n1 - 1 else (quot n 2)

