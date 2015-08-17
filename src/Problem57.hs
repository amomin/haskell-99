module Problem57 where

import NNPTrees

construct :: (Ord a) => [a] -> Tree a
construct [] = Empty
construct xs = insert Empty xs
    where
        insert :: (Ord a) => Tree a -> [a] -> Tree a
        insert x [] = x
        insert Empty (x:xs) = insert (Branch x Empty Empty) xs
        insert (Branch m l r) (x:xs)
            | compare m x == GT = insert (Branch m (insert l [x]) r) xs
            | otherwise = insert (Branch m l (insert r [x])) xs

-- similar but nicer solution from posted solutions
construct2 xs = foldl add Empty xs
    where
        add :: (Ord a) => Tree a -> a -> Tree a
        add Empty x = Branch x Empty Empty
        add (Branch m l r) x
            | compare m x == GT = Branch m (add l x) r
            | otherwise = Branch m l (add r x)
