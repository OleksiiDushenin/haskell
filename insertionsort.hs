sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort xs = insert [] xs

insert :: (Ord a) => [a] -> [a] -> [a]
insert leftxs [] = leftxs  
insert leftxs (x:rightxs) = insert (lxs ++ [x] ++ rxs) rightxs
                            where 
                                lxs = [a | a <- leftxs, a < x]
                                rxs = [a | a <- leftxs, a >=x]

main = print (sort [5, 3, 7, 8, 1, 2])

