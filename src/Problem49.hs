module Problem49 where

-- I think this problem showed up on programming praxis 
-- at some point

-- slower than necessary because of the reversals
gray1 :: Int -> [String]
gray1 0 = []
gray1 1 = ["0","1"]
gray1 n = (map ('0':) prevGray) ++ (map ('1':) $ reverse prevGray)
    where
        prevGray = gray1 (n-1)

-- just copied from solution
-- but this seems to be wrong
-- (doesn't the order of "0" ++ and "1" ++ need to alternate?)
gray2 :: Integral a => a -> [String]
gray2 0 = [""]
gray2 n = foldr (\s acc -> ("0" ++ s):("1" ++ s):acc) [] $ gray2 (n-1)
