module Problem23 where

import System.Random (randomRIO)
import Problem20 (removeAt)

--pick1 xs = randomRIO(1,(length xs)) >>= return . (xs !! )

rndSelect23 :: [a] -> Int -> IO [a]
rndSelect23 _ n | (n < 1) = error "Input must be positive"
rndSelect23 lst n = 
    let
        pickPos :: [a] -> IO Int
        pickPos xs = randomRIO(0,(length xs) - 1)
        pick :: [a] -> IO a
        pick xs = pickPos xs >>= return . (xs !! )
    in
    if (null lst)
        then do
            return []
        else
            if n == 1
                then do 
                    x <- pick lst
                    return [x]
                else do
                    pos <- pickPos lst
                    let (x, reducedList) = removeAt (pos + 1) lst
                    ys <- rndSelect23 reducedList (n-1)
                    return (x:ys)
