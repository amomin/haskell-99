module Problem32 where

myGCD :: Int -> Int -> Int
myGCD a b = myGCD' (max a b) (min a b)
    where
        myGCD' big small
            | small == 0 = abs big
            | otherwise = myGCD' small (mod big small)
