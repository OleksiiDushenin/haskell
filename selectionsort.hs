sort :: (Ord a) => [a] -> [a]
sort [] = []
sort as = minEl:sort remaining
            where 
                minEl = minInList as
                remaining = remove as minEl 

minInList :: (Ord a) => [a] -> a
minInList [x] = x
minInList (x:xs) = minInListWithElement xs x

minInListWithElement :: (Ord a) => [a] -> a -> a
minInListWithElement [] y = y
minInListWithElement (x:xs) y = minInListWithElement xs (minEl y x)

minEl :: (Ord a) => a -> a -> a
minEl x y  
        | y < x = y
        | otherwise = x

remove :: (Ord a) => [a] -> a -> [a]
remove [] y = []
remove (x:xs) y
        | x == y = remove xs y
        | otherwise = x:remove xs y

main = print (sort [6, 10, 5, 3, 1, 7, 9])

