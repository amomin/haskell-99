module Problem20 where

removeAt :: Int -> [a] -> (a, [a])
removeAt n [] = error "Empty list"
-- ignoring handling the case s is empty
removeAt n xs 
    | n < 1 = error "Out of range"
    | otherwise =   let (f,s) = splitAt (n-1) xs
                    in
                        if (null s)
                            then error "Out of range"
                            else
                            ((head s), f ++ tail s)
