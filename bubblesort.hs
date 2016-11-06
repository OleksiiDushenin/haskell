sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort xs = bubble xs (length xs)

bubble :: (Ord a) => [a] -> Int -> [a]
bubble xs n
    | n == 0 = xs
    | otherwise = bubble (bubble_iteration xs) (n - 1)


bubble_iteration :: (Ord a) => [a]  -> [a]
bubble_iteration [] = []
bubble_iteration [x] = [x]
bubble_iteration (x:y:xs)
    | y < x = [y] ++ bubble_iteration ([x] ++ xs)
    | otherwise = [x] ++ bubble_iteration ([y] ++ xs)

main = print (sort [5, 3, 7, 8, 1, 2])

