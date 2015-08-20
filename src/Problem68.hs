module Problem68 where

import NNPTrees

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Branch x l r)
    = (x:) $ (preOrder l) ++ (preOrder r)

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Branch x l r)
    = (inOrder l) ++ [x] ++ (inOrder r)

-- given the preorder
-- and inorder
-- construct the tree
-- e.g.
-- preInOrder "abc" "bac" = a(b,c)
preInOrder :: (Eq a) => [a] -> [a] -> Tree a
preInOrder [] _ = Empty
preInOrder [x] _ = Branch x Empty Empty
preInOrder po@(hd1:tl1) io@(hd2:tl2)
    | hd1 == hd2 = Branch hd1 Empty (preInOrder tl1 tl2)
preInOrder po@(hd:tl) io
    = (Branch hd
        (preInOrder lpo lio)
        (preInOrder rpo rio)
      )
        where
            lio = takeWhile (/= hd) io
            (_:rio) = dropWhile (/= hd) io
            lst = last lio
            lpo = (takeWhile (/= lst) tl) ++ [lst]
            (_:rpo) = dropWhile (/= lst) tl
