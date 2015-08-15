module Problem39 where

import Problem31(primesUpTo)

primesR :: Int -> Int -> [Int]
primesR a b = dropWhile (< a) $ primesUpTo b
