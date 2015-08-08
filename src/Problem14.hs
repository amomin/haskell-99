module Problem14 where

duplicate1 :: [a] -> [a]
duplicate1 [] = []
duplicate1 (x:xs) = x:x:duplicate1(xs)

-- using foldl
duplicate2 :: [a] -> [a]
duplicate2 x = reverse $ foldl (\acc y -> y:y:acc) [] x

-- using foldr, more naturally (no need to reverse)
duplicate3 :: [a] -> [a]
duplicate3 x = foldr (\el acc -> el:el:acc ) [] x

