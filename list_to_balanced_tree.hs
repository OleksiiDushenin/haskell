data Tree a = Leaf a | Node (Tree a) (Tree a)
       deriving Show

balance :: [a] -> Tree a
balance [a] = Leaf a
balance xs = Node (balance (fst parts)) (balance (snd parts))
                        where parts = split xs [] []

split :: [a] -> [a] -> [a] -> ([a], [a])
split [] ys zs = (ys, zs)
split (x:xs) ys zs  = split xs (zs ++ [x]) ys

main = print(balance [1, 2, 3, 4, 5, 6, 7, 8, 9])
