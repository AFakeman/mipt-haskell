{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Text.Printf
import Data.List

-- Task 1
t1_a :: [([(Int, Int -> Int)], Char)]
t1_a = [([(4, \x -> x + 1)],'a')]

t1_b :: [(Char, Int)] -> ([Int], [Char])
t1_b lst = (map snd lst, map fst lst)

t1_c :: a -> (a, a)
t1_c val = (val, val)

t1_d :: (a -> b) -> a -> b
t1_d f x = f x

-- Task 2

-- Types should be like that because the function accepts a tuple, and returns
-- a tuple. Type should be swapped in the output. This is the most general
-- option.
t2_e1 :: (a, b) -> (b, a)
t2_e1 (a, b) = (b, a)

-- y x is application of y to x. Return value can be arbitrary.
t2_e2 :: (a, a -> b) -> b
t2_e2 (x, y) = y x

-- The reverse of the previous problem
t2_e3 :: (a -> b, a) -> b
t2_e3 (x, y) = x y

-- Same, but without a tuple
t2_e4 :: a -> (a -> b) -> b
t2_e4 x y = y x

-- This one requires an infinite type, because x should be a function
-- that accepts itself. That reqires Haskell to construct an infinite function.
-- t2_e5 x y = x y x

-- Task 3
t3_a :: Bool -> Bool -> Bool
t3_a True True = True
t3_a _ _ = False

t3_b :: Bool -> Bool -> Bool
t3_b True False = False
t3_b _ _ = True

t3_c :: Bool -> Bool -> Bool
t3_c True True = False
t3_c True False = True
t3_c False x = x

t3_d :: Bool -> Bool -> Bool -> Bool
t3_d True True _ = True
t3_d False False _ = False
t3_d True False x = x
t3_d False True x = x

-- Task 4
replicate' n a = [a | _ <- [1..n]]
repeat' a = [a | _ <- [1..]]

-- Task 5
t5 = [(x, y, z) | z <- [1..], x <- [1..z], y <- [1..z], z * z == x * x + y * y ]

-- Task 6
t6_helper n = sum [x | x <- [1..n - 1], mod n x == 0]
t6 x y = (t6_helper x) == y && (t6_helper x) == y

-- Task 7
t7 = scanl (+) 1 [2..]

-- Task 8
type Bfn = Bool -> Bool -> Bool -> Bool

bfnZero :: Bfn
bfnZero x y z = False

bfnOne :: Bfn
bfnOne x y z = True

bfnSum :: Bfn -> Bfn -> Bfn
bfnSum f g = \x y z -> t3_c (f x y z) (g x y z)

bfnProd :: Bfn -> Bfn -> Bfn
bfnProd f g = \x y z -> (f x y z) && (g x y z)

bfnNeg :: Bfn -> Bfn
bfnNeg = id

bfnEq :: Bfn -> Bfn -> Bool
bfnEq f g = and [f x y z == g x y z | x <- tf, y <-tf, z <- tf]
    where tf = [True, False]

bfnFmt :: Bfn -> Bool -> Bool -> Bool -> String
bfnFmt f x y z = printf "%s %s %s -> %s" (bf x) (bf y) (bf z) (bf r)
    where r = f x y z
          bf True = "0"
          bf False = "1"

instance Eq Bfn where
    (==) = bfnEq

instance Num Bfn where
    (+) = bfnSum
    (*) = bfnProd
    negate = bfnNeg

instance Show Bfn where
    show f = intercalate ", "
                [bfnFmt f x y z| x <- tf,
                                 y <- tf,
                                 z <- tf]
        where bf True = "0"
              bf False = "1"
              tf = [True, False]
