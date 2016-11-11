data Tree a = Leaf a | Node (Tree a) (Tree a)

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) =  balanced l && balanced r && abs (height l - height r) <= 1

height :: Tree a -> Int
height (Leaf _) = 1
height (Node l r) = 1 + max (height l) (height r)

main = print(balanced (Node (Node (Leaf 5) (Leaf 6)) (Node (Leaf 3) (Leaf 4))))
