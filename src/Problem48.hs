module Problem48 where

import Problem46
import Data.Bits

tablen :: Int -> ([Bool] -> Bool) -> IO()
tablen n f = mapM_ putStrLn [myShow vars ++ show (f vars) | vars <- getTableVars n]
    where
        getTableVars :: Int -> [[Bool]]
        getTableVars nVars = map (\y -> map (\(x) -> testBit (y::Int) x) [0..(nVars-1)]) [0..(2^nVars -1)]
        myShow :: [Bool] -> String
        myShow [] = " "
        myShow (b:bs) = if b then (show b) ++ "  " ++ myShow bs else (show b) ++ " " ++ myShow bs

-- alternate implementations of getTableVars
getTableVars1 :: Int -> [[Bool]]
getTableVars1 0 = []
getTableVars1 1 = [[False], [True]]
getTableVars1 n = concatMap (\x -> [False:x, True:x]) (getTableVars1 (n-1))
--getTableVars1 n = (map (False:) $ getTableVars1 (n-1)) ++ (map (True:) $ getTableVars1 (n-1))

