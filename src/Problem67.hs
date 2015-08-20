module Problem67 where

import NNPTrees

treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r)
    = x:"("
        ++ (treeToString l)
        ++ ","
        ++ (treeToString r)
        ++ ")"

treeToStringAllTypes :: (Show a) => Tree a -> String
treeToStringAllTypes Empty = ""
treeToStringAllTypes (Branch x Empty Empty) = show x
treeToStringAllTypes (Branch x l r)
    = (show x) ++ "("
        ++ (treeToStringAllTypes l)
        ++ ","
        ++ (treeToStringAllTypes r)
        ++ ")"

-- gave up and looked at solution - this
-- is essentially a copy
stringToTree :: String -> Tree Char
stringToTree "" = Empty
stringToTree [x] = Branch x Empty Empty
stringToTree str = fst $ helper str
  where
      helper :: String -> (Tree Char, String)
      helper (hd:tl)
        | (hd == ',' || hd == ')') = (Empty,hd:tl)
      helper (hd1:hd2:tl)
        | (hd2 == ',' || hd2 == ')')
          = ((Branch hd1 Empty Empty),hd2:tl)
        | hd2 == '(' 
          = ((Branch hd1 l r), xs2)
            where
              (l, ',':xs1) = helper tl
              (r, ')':xs2) = helper xs1
                        
