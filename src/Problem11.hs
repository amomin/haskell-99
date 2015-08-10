module Problem11 where

import Problem09 (pack)
import Problem10 (encode)

data Record a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Record a]
encodeModified lst = 
    let
        reducer x
            | len == 1 = Single hed
            | otherwise = Multiple len hed
            where
                len = length x
                hed = head x
        
    in
        map reducer (pack lst)

-- From provided solutions

-- duh, could have just mapped encode solution
encodeModified1 :: (Eq a) => [a] -> [Record a]
encodeModified1 = map reducer . encode
    where
        reducer (1, x) = Single x
        reducer (n, x) = Multiple n x
