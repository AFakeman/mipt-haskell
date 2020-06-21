-- Missing:
-- Task 36
-- Task 37
-- Task 39
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

-- Task 36
insAVL :: (Ord a) => a -> Tree a -> Tree a
insAVL a = fst . (insAVLHelper a)

insAVLHelper :: (Ord a) => a -> Tree a -> (Tree a, Int)
insAVLHelper x Nil = (Node Nil x Nil, 1)
insAVLHelper x (Node l y r)
    | x <= y = let (newL, leftDepth) = insAVLHelper x l
                   rightDepth = treeDepth r
                   newTree = Node newL y r in
                   if leftDepth > rightDepth + 1 then
                       (rotateRight newTree, leftDepth)
                   else
                       (newTree, leftDepth + 1)
    | x > y = let (newR, rightDepth) = insAVLHelper x r
                  leftDepth = treeDepth l
                  newTree = Node l y newR in
                  if rightDepth > leftDepth + 1 then
                      (rotateLeft newTree, rightDepth)
                  else
                      (newTree, rightDepth + 1)


treeDepth :: Tree a -> Int
treeDepth = foldt (\x _ z -> (max x z) + 1) 0

rotateRight :: Tree a -> Tree a
rotateRight (Node (Node ll lx lr) x r) = Node ll lx (Node lr x r)

rotateLeft :: Tree a -> Tree a
rotateLeft (Node l x (Node rl rx rr)) = Node (Node l x rl) rx rr

-- Task 38
data Cmpr a = Sg a | Mlt a Int
    deriving (Show)

compress_helper :: Eq a => [Cmpr a] -> a -> [Cmpr a]
compress_helper [] y  = [Sg y]
compress_helper ((Sg x) : xs) y
    | x == y = (Mlt x 2):xs
    | otherwise = (Sg y):(Sg x):xs
compress_helper ((Mlt x c):xs) y
    | x == y = (Mlt x (c + 1)):xs
    | otherwise = (Sg y):(Mlt x c):xs

compress :: Eq a => [a] -> [Cmpr a]
compress = reverse . foldl compress_helper []

decompress_helper :: Cmpr a -> [a]
decompress_helper (Sg x) = [x]
decompress_helper (Mlt x c) = replicate c x

decompress :: [Cmpr a] -> [a]
decompress = concat . map decompress_helper
