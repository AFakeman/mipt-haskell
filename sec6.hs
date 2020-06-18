import Data.Tuple

-- Task 21
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\y ys -> (f y):ys) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\y ys -> if f y then y:ys else ys) []

and' :: [Bool] -> Bool
and' = foldr (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr ((||) . f) False

all' :: (a -> Bool) -> [a] -> Bool
all' f = foldr ((&&) . f) True

-- Task 22
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x:(takeWhile' f xs)
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs)
    | f x = (dropWhile' f xs)
    | otherwise = x:xs

-- Task 23
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f xs = foldr f (last xs) (init xs)

-- Task 24
lmax :: Ord a => [a] -> a
lmax xs = foldr1 (max) xs

-- Task 25
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' _ s [] = [s]
scanl' f s (x:xs) = s : (scanl' f (f s x) xs)

-- Task 26
prefixes :: [a] -> [[a]]
prefixes xs = drop 1 $ map reverse $ scanl (\x y -> y:x) [] xs

-- Task 27
rot :: [a] -> [a]
rot xs = (last xs) : (init xs)

rotts :: [a] -> [[a]]
rotts xs = take (length xs) (iterate rot xs)

rotts' :: [a] -> [[a]]
rotts' xs = init $ scanl (\ys _ -> rot ys) xs xs

-- Task 28
unzip' :: [(a, b)] -> ([a], [b])
unzip' xs = (map fst xs, map snd xs)

unzipper :: (a, b) -> ([a], [b]) -> ([a], [b])
unzipper (x, y) (xs, ys) = (x:xs, y:ys)
unzip'' :: [(a, b)] -> ([a], [b])
unzip'' lst = foldr unzipper ([], []) lst

-- Task 29
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

-- Task 30
-- g :: a -> b -> d
-- h :: d -> c
-- f :: a -> b -> c
-- f x y = h(g x y)
-- uncurry g :: (a, b) -> d
-- h . (uncurry g) :: (a, b) -> c
-- f = curry $ h . (uncurry g)

-- Task 31
zapp :: [a -> b] -> [a] -> [b]
zapp = curry ((map (uncurry ($))) . (uncurry zip))

-- Task 32
compress_helper :: Eq a => [(a, Int)] -> a -> [(a, Int)]
compress_helper [] y  = [(y, 1)]
compress_helper ((x, c):xs) y
    | x == y = (x, c + 1):xs
    | otherwise = (y, 1):(x, c):xs

compress :: Eq a => [a] -> [(a, Int)]
compress = reverse . foldl compress_helper []

decompress_helper :: (a, Int) -> [a]
decompress_helper = (uncurry replicate) . swap

decompress :: [(a, Int)] -> [a]
decompress = concat . map decompress_helper
