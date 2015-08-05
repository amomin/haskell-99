module Problem01 where

main::IO()
main = do
    putStrLn "Type myLast [list]"
    
myLast::[a] -> a
myLast [] = error "List cannot be empty"
myLast [x] = x
myLast (_:xs) = myLast xs