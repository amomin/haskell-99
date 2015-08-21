module Problem46 where

myNot :: Bool -> Bool
myNot True = False
myNot False = True

myAnd :: Bool -> Bool -> Bool
--myAnd = (&&)
myAnd = myAnd1

myOr :: Bool -> Bool -> Bool
--myOr = (||)
myOr = myOr1

myNand :: Bool -> Bool -> Bool
myNand x y = myNot $ myAnd x y

myNor :: Bool -> Bool -> Bool
myNor x y = myNot $ myOr x y

myXor :: Bool -> Bool -> Bool
myXor = myXor1

myImpl :: Bool -> Bool -> Bool
myImpl x y = myOr (myNot x) y

myEqu :: Bool -> Bool -> Bool
myEqu = (==)

table :: (Bool -> Bool -> Bool) -> IO()
table f = do
            print (concat ["True True ", show (f True True)])
            print (concat ["True False ", show (f True False)])
            print (concat ["False True ", show (f False True)])
            print (concat ["False False ", show (f False False)])

-- more concise (cheated - looked at solution)
table1 f = mapM_ putStrLn [show x ++ " " ++ show y ++ " " ++ show (f x y) | x <- [True, False], y <- [True, False]]

myAnd1 :: Bool -> Bool -> Bool
myAnd1 True True = True
myAnd1 _ _ = False

myOr1 :: Bool -> Bool -> Bool
myOr1 False False = False
myOr1 _ _ = True

myXor1 :: Bool -> Bool -> Bool
myXor1 False False = False
myXor1 True True = False
myXor1 _ _ = True

myAnd2 :: Bool -> Bool -> Bool
myAnd2 x y = and [x,y]

myOr2 :: Bool -> Bool -> Bool
myOr2 x y = or [x,y]

