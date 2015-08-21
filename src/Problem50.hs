module Problem50 where

import NNPTrees

huffman :: [(Char,Int)] -> [(Char,String)]
huffman = decodeTree . buildTree
    where

        decodeTree :: Tree (Maybe Char, Int) -> [(Char,String)]
        decodeTree tree = hlp tree ""
            where
                hlp Empty _ = []
                hlp (Branch (Nothing,_) l r) prefix 
                    = (hlp l (prefix ++ "0")) ++ (hlp r (prefix ++ "1"))
                hlp (Branch (Just x,n) l r) prefix 
                    = ((x,prefix):) $ hlp l (prefix ++ "0") ++  hlp r (prefix ++ "1")

        buildTree :: [(Char,Int)] -> Tree (Maybe Char, Int)
        buildTree xs = hlp $ map (\(x,n) -> Branch (Just x,n) Empty Empty) $ srt xs
            where
                hlp :: [Tree (Maybe Char, Int)] -> Tree (Maybe Char, Int)
                hlp [x] = x
                hlp priorityQueue@(ln1:ln2:tl) = hlp $ 
                    insrt 
                        (Branch (Nothing,(nodeVal ln1) + (nodeVal ln2)) ln1 ln2)
                        tl
                insrt t [] = [t]
                insrt t@(Branch (_,m) _ _) pq@(hd@(Branch (x,n) _ _):xs)
                    | m < n = t:pq
                    | otherwise = hd:(insrt t xs)
                srt :: [(Char,Int)] -> [(Char,Int)]
                srt [] = []
                srt list = let x = (head list) in
                    (srt (filter (compGreaterThan x) list))
                    ++ 
                    (filter (compEq x) list)
                    ++ 
                    (srt (filter (compLessThan x) list))
                compLessThan (_,m) (_,n)        = m < n
                compGreaterThan (_,m) (_,n)     = m > n
                compEq (_,m) (_,n)              = m == n
                nodeVal Empty = 0
                nodeVal (Branch (_,m) _ _) = m
