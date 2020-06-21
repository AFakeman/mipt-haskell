import Data.Char
import System.IO
import Control.Exception
import System.Random
--- Task 40

getInt :: IO Int
getInt = do l <- getLine
            return (read l)

getInts :: Int -> IO [Int]
getInts n | n < 0       = error "Negative argument number!"
          | n == 0      = return []
          | otherwise   = do x <- getInt
                             xs <- getInts (n-1)
                             return (x:xs)

operationForString :: String -> Maybe ([Int] -> Int)
operationForString "+" = Just sum
operationForString "*" = Just (foldr (*) 1)
operationForString _ = Nothing

getOperation ::  IO ([Int] -> Int)
getOperation = do putStr "Which operation?"
                  l <- getLine
                  let op = operationForString l
                  case op of Just f -> return f
                             Nothing -> getOperation

adder :: IO()
adder = do  putStr "How many numbers? "
            n <- getInt
            xs <- getInts n
            op <- getOperation
            putStr "The total is "
            putStrLn (show (op xs))

-- Task 41

maxString :: String -> IO String
maxString p = do l <- getLine
                 if l == "END" then
                     return p
                 else
                     maxString $ max l p

maxStringMain :: IO()
maxStringMain = (maxString "") >>= putStrLn

-- Task 42
toLowerMain :: IO()
toLowerMain = do eof <- isEOF
                 if not eof then
                     do l <- getLine
                        putStr $ map toLower l
                        toLowerMain
                 else
                     return ()

-- Task 43
-- See task43.hs

-- Task 44
readNumbers :: IO()
readNumbers = do l <- getLine
                 let r = reads l :: [(Integer, String)]
                 if null r then
                     return ()
                 else
                     readNumbers

-- Task 45
popFromList :: Int -> [a] -> [a]
popFromList i xs
    | length xs > i = (\(ys, (z:zs)) -> (ys ++ zs)) $ (splitAt i xs)
    | otherwise = error "Index too big"

listShufflePure :: RandomGen g => [a] -> g -> [a]
listShufflePure [] _ = []
listShufflePure xs gen = let (idx, newGen) = randomR (0, length xs - 1) gen
                             x = xs !! idx
                             rest = popFromList idx xs
                             shuffledRest = listShufflePure rest newGen
                         in x:shuffledRest

listShuffle :: [a] -> IO([a])
listShuffle [] = return []
listShuffle xs = listShufflePure xs <$> getStdGen

-- Task 46
qsplit :: Ord a => Int -> [a] -> ([a], a, [a])
qsplit at xs = (le, x, gt)
    where (left, (x:right)) = splitAt at xs
          rest = left ++ right
          le = filter (<= x) rest
          gt = filter (> x) rest

kthRandomPure :: (Ord a, RandomGen gen) => Int -> [a] -> gen -> a
kthRandomPure k xs g
    | pos == k = x
    | pos > k = kthRandomPure k le newGen
    | pos < k = kthRandomPure (k - pos) gt newGen
    where (at, newGen) = randomR (0, length xs - 1) g
          pos = length le + 1
          (le, x, gt) = qsplit at xs

kth :: Ord a => Int -> [a] -> IO a
kth k xs = kthRandomPure k xs <$> getStdGen
