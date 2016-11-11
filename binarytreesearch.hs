data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = case (compare x y) of
                        EQ -> True
                        otherwise -> False
occurs x (Node l y r) = case (compare x y ) of
                            EQ -> True
                            LT -> occurs x l
                            GT -> occurs x r

main = print (occurs 5 (Node (Node (Leaf 1) 15 (Leaf 7)) 10 (Leaf 12) ))
