module Problem64 where

import NNPTrees

layout :: Tree a -> Tree (a,(Int,Int))
layout = layout2

-- keep track of size in the helper
layout2 :: Tree a -> Tree (a,(Int,Int))
layout2 Empty = Empty
layout2 t = fst $ helper t 1 1
    where
        helper :: Tree a1 -> Int -> Int -> (Tree (a1,(Int,Int)),Int)
        helper Empty _ _ = (Empty, 0)
        helper (Branch x l r) offset height = (Branch 
                                                (x,(offset + sizel,height)) 
                                                left
                                                right
                                              , 1 + sizel + sizer)
            where
                (left, sizel) = helper l offset (height + 1)
                (right, sizer) = helper r (offset + sizel + 1) (height + 1)

-- uses size function which will slow things down
layout1 :: Tree a -> Tree (a,(Int,Int))
layout1 Empty = Empty
layout1 t = helper t 1 1
    where
        helper :: Tree a1 -> Int -> Int -> Tree (a1,(Int,Int))
        helper Empty _ _ = Empty
        helper (Branch x l r) offset height = Branch 
                                                (x,(offset + (size l),height)) 
                                                (helper l offset (height + 1))
                                                (helper r (offset + (size l) + 1) (height + 1))
        size Empty = 0
        size (Branch _ l r) = 1 + size l + size r
