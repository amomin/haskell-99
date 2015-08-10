module Problem17 where

split1 :: [a] -> Int -> ([a],[a])
split1 [] _ = ([], [])
split1 x@(hd:tl) n
    | n <= 0 = ([], x)
    | otherwise = (hd:first, second)
        where
            (first, second) = split1 tl (n-1)

-- solution asks to use no "predicates", otherwise
-- the following would be valid

split2 :: [a] -> Int -> ([a],[a])
split2 = flip splitAt

split3 :: [a] -> Int -> ([a],[a])
split3 lst n = (take n lst, drop n lst)
