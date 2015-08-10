module Problem21 where

insertAt :: a -> [a] -> Int -> [a]
insertAt _ xs n
    | n < 1 = error "Out of range"
    | (null xs) && n > 1 = error "Out of range"
insertAt x xs 1 = (x:xs)
insertAt x xs@(hd:tl) n = hd:(insertAt x tl (n-1))
