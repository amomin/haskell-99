module Problem19 where

rotate1 :: [a] -> Int -> [a]
rotate1 xs a = let
    (first, second) = splitAt amod xs
        where
            amod = (mod a (length xs))
    in
        second ++ first
