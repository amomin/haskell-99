module Problem63 where

import NNPTrees

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = completeBinaryTreeTyped 'x' n

completeBinaryTreeTyped :: a -> Int -> Tree a
completeBinaryTreeTyped val n = helper 1 n
    where
        helper cNode maxNode
            | 2*cNode + 1 <= maxNode     = Branch val (helper (2*cNode) maxNode) (helper (2*cNode+1) maxNode) 
            | 2*cNode <= maxNode         = Branch val (helper (2*cNode) maxNode) Empty
            | cNode <= maxNode           = Branch val Empty Empty
            | otherwise                  = Empty

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree = isCompleteBinaryTree2

-- crappyish solution
-- compare with complete binary tree
isCompleteBinaryTree1 :: Tree a -> Bool
isCompleteBinaryTree1 t = isomorphic t $ completeBinaryTree $ countNodes t
    where
        countNodes :: Tree a -> Int
        countNodes Empty = 0
        countNodes (Branch _ l r) = 1 + (countNodes l) + (countNodes r)
        isomorphic :: Tree a1 -> Tree b1 -> Bool
        isomorphic Empty Empty = True
        isomorphic _ Empty = False
        isomorphic Empty _ = False
        isomorphic (Branch _ x1 y1) (Branch _ x2 y2) = and [isomorphic x1 x2, isomorphic y1 y2]

-- inspired by the posted solution but slightly different
-- count number of nodes, and keep track of maximum "position"
-- achieved by an element of the tree (using the numbering scheme
-- as in the completeBinaryTree algorithm)
isCompleteBinaryTree2 :: Tree a -> Bool
isCompleteBinaryTree2 t = let (x,y) = helper t 1 in x == y
    where
        -- above is true
        -- return (number of elements, max position achieved)
        helper Empty n = (0,0)
        helper (Branch _ l r) n = (1 + szl + szr, max n $ max nl nr)
            where
                (szl,nl) = helper l (2*n)
                (szr,nr) = helper r (2*n + 1)
