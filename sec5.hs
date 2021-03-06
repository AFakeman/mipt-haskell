import Control.Monad
-- Missing:
-- Task 13

-- Task 9
head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> a
tail' [x] = x
tail' (x:xs) = tail' xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : (take' (n - 1) xs)

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n - 1) xs

null' :: [a] -> Bool
null' [] = False
null' _ = True

elem' :: Eq a => a -> [a] -> Bool
elem' y [] = False
elem' y (x:xs) = (y == x) || (elem' y xs)

idx' :: [a] -> Int -> a
idx' (x:xs) 0 = x
idx' (x:xs) n = idx' xs (n - 1)

plusHelper' :: [a] -> [a] -> [a]
plusHelper' xs [] = xs
plusHelper' [] ys = ys
plusHelper' (x:xs) ys = plusHelper' xs (x:ys)

plus' :: [a] -> [a] -> [a]
plus' xs ys = plusHelper' (reverse xs) ys

concat' :: [[a]] -> [a]
concat' [] = []
concat' xs = foldl plus' [] xs

-- Task 10
slice :: Int -> Int -> [a] -> [a]
slice l r xs = take (r - l) (drop l xs)

-- This even supports infinite lists. However,
-- since we don't check length, on finite lists at some point r overflows,
-- and we start returning empty lists.
segsHelper :: [a] -> [[a]]
segsHelper xs = [s | r <- [1..],
                     l <- [0..(r - 1)],
                     let s = slice l r xs,
                     length s == (r - l)]
segs :: [a] -> [[a]]
segs xs = takeWhile (not . null) (segsHelper xs)

-- Task 11
gcd' :: Int -> Int -> Int
gcd' x y
    | y == 0 = x
    | x >= y = gcd' y (mod x y)
    | y >= x = gcd' x (mod y x)

gcd'' x y
    | y == 0 = x
    | x >= y = gcd'' (x - y) y
    | y >= x = gcd'' (y - x) x

-- Task 12
egcd :: Int -> Int -> (Int, Int, Int)
egcd x y
   | y == 0 = (abs x, 1, 0)
   | otherwise = (gcd, signMod * a, signMod * b)
   where
       (gcd, suba, subb) = egcd y (mod x y)
       a = subb
       b = suba - (div x y) * subb
       signMod = signum (x * a + y * b)

-- Task 13

solveDiophantine :: Int -> Int -> Int -> [(Int, Int)]
solveDiophantine a b c
    | mod c g == 0 = [(x * c' - b' * t, y * c' + a' * t) | i <- [1..],
                                                           t <- [-i, i]]
    | otherwise = []
    where (g, x, y) = egcd a b
          c' = div c g
          a' = div a g
          b' = div b g

-- Task 14
dupl :: [a] -> [a]
dupl [] = []
dupl (x:xs) = [x,x] ++ dupl xs

-- Task 15
nrem :: Int -> [a] -> [a]
nrem_helper n x = mod (snd x) n /= 0
nrem n xs = map fst (filter (nrem_helper n) (zip xs [1..]))

-- Task 16
duprem_helper :: Eq a => [a] -> [a] -> [a]
duprem_helper u [] = u
duprem_helper u (x:xs)
    | elem x u = duprem_helper u xs
    | otherwise = duprem_helper (x:u) xs

duprem :: Eq a => [a] -> [a]
duprem xs = reverse $ duprem_helper [] xs

-- Task 17
qsplit :: Ord a => [a] -> ([a], a, [a])
qsplit (x:xs) = (le, x, gt)
    where le = filter (<= x) xs
          gt = filter (> x) xs

kth :: Ord a => Int -> [a] -> a
kth k xs
    | pos == k = x
    | pos > k = kth k le
    | pos < k = kth (k - pos) gt
    where (le, x, gt) = qsplit xs
          pos = length le + 1

-- Task 18
part :: Int -> Int -> [[Int]]
part 1 n = [[n]]
part m n = [(k:ks) | k <- [1..n-1], ks <- part (m -1) (n - k)]

-- Task 19
sbseqs :: [a] -> [[a]]
sbseqs [] = []
sbseqs xs = filterM (const [True, False]) xs

-- Task 20
sieve_helper :: [Integer] -> [Integer]
sieve_helper [] = []
sieve_helper (x:xs) = x : sieve_helper [y | y <- xs, mod y x /= 0 ]

sum_helper n = [ (x, y) | x <- sieve_helper [2..n],
                          y <- sieve_helper [2..x],
                          x + y == n ]

sum_split :: Integer -> (Integer, Integer)
sum_split n = head $ sum_helper n
