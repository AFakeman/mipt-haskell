data Nat = Z | S Nat
    deriving (Eq, Ord, Show)

instance Num Nat where
    m + Z = m
    m + (S n) = S (m + n)

    _ * Z = Z
    m * (S n) = m + m * n

    Z - _ = Z
    m - Z = m
    (S m) - (S n) = m - n

    fromInteger n
        | n == 0 = Z
        | n > 0 = S $ fromInteger (n - 1)

    negate _ = Z
    signum Z = Z
    signum (S _) = S Z
    abs = id

foldn :: (a -> a) -> a -> Nat -> a
foldn f x Z = x
foldn f x (S n) = f $ foldn f x n

-- Task 33

fib :: Nat -> Nat
fib Z = Z
fib (S Z) = S Z
fib (S (S n)) = (fib n) + (fib (S n))

fib_helper :: (Nat, Nat) -> (Nat, Nat)
fib_helper (x, y) = (y, x + y)

fib' :: Nat -> Nat
fib' = fst . foldn fib_helper (Z, S Z)

-- Task 34
instance Enum Nat where
    toEnum 0 = Z
    toEnum n
        | n > 0 = S $ toEnum (n - 1)

    fromEnum Z = 0
    fromEnum (S n) = 1 + fromEnum n

instance Real Nat where
    toRational = toRational . toInteger

instance Integral Nat where
    mod x y
        | x >= y = mod (x - y) y
        | otherwise = x

    div x y
        | x >= y = 1 + div (x - y) y
        | otherwise = 0

    toInteger Z = 0
    toInteger (S n) = 1 + toInteger n
    quotRem m n = (div m n, mod m n)

-- Task 35
data Tree a = Nil | Node (Tree a) a (Tree a)
    deriving (Eq,Show)
foldt :: (b -> a -> b -> b) -> b -> Tree a -> b
foldt f x Nil = x
foldt f x (Node l y r) = f (foldt f x l) y (foldt f x r)

t35 :: Tree Int -> Int
t35 = foldt (\x y z -> x + y + z) 0
