module Problem02 where

myButLast::[a] -> a
myButLast [] = error "List must have at least two elements"
myButLast [_] = error "List must have at least two elements"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

