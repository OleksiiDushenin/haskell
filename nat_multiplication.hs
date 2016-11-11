data Nat = Zero | Suc Nat
    deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Suc m) n = Suc (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Suc m) n = add n (mult m n)

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Suc (intToNat (n - 1))

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Suc n) = 1 + natToInt(n)

main = print (natToInt (mult (intToNat 15) (intToNat 5)))
