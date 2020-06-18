import Control.Monad
-- Missing:
-- Task 12
-- Task 13
-- Task 17
-- Task 20

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

-- Task 18
part :: Int -> Int -> [[Int]]
part 1 n = [[n]]
part m n = [(k:ks) | k <- [1..n-1], ks <- part (m -1) (n - k)]

-- Task 19
sbseqs :: [a] -> [[a]]
sbseqs [] = []
sbseqs xs = filterM (const [True, False]) xs

-- Task 20
