data Tree a = Leaf | Node (Tree a) a (Tree a)
                deriving Show

instance Functor Tree
    where
        -- fmap :: (a -> b) -> f a -> f b
        fmap _ Leaf = Leaf
        fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

getTree :: Tree Int
getTree = Node (Node Leaf 5 Leaf) 3 Leaf

main = print (fmap (+7) getTree)
