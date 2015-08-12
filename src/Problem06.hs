module Problem06 where

isPalindrome1 :: (Eq a) => [a] -> Bool
isPalindrome1 x = x == reverse x

isPalindrome2 :: (Eq a) => [a] -> Bool
isPalindrome2 lst = helper lst lst []
    where
        helper :: (Eq a) => [a] -> [a] -> [a] -> Bool
        helper x [] z = x == z
        helper x (y:ys) z = helper x ys (y:z)

--Not mine, inspired by the solution
--uses half as many compares
isPalindrome3 :: (Eq a) => [a] -> Bool
isPalindrome3 lst = helper lst lst []
    where
        helper :: (Eq a) => [a] -> [a] -> [a] -> Bool
        helper x [] z = x == z
        helper x [_] z = x == (head x):z
        helper (x:xs) (_:_:ys) z = helper xs ys (x:z)
