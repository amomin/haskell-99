module Problem10 where

import Problem09 (pack)

encode :: (Eq a) => [a] -> [(Int, a)]
encode lst = map (\x -> ((length x), (head x))) (pack lst)
