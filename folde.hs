data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

eval :: Expr -> Int
eval ex = folde (\x -> x) (\x -> \y -> x + y) ex

size :: Expr -> Int
size ex = folde (\_ -> 1) (\x -> \y -> x + y) ex

main = print (size(Add (Add (Val 5) (Val 2)) (Val 5)))
