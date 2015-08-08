module Problem12 where

import Problem11

decodeModified :: [Record a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = (replicate 1 x) ++ (decodeModified xs)
decodeModified ((Multiple n x):xs) = (replicate n x) ++ (decodeModified xs)
