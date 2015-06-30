{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -O2 #-}
import Data.List

foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = \x -> f (g x)

-- convert them to wholemeal funcs
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum
      . filter even
      . takeWhile (/= 1)
      . iterate (\n -> if even n then n `div` 2 else 3*n+1)

-- binary tree
type Height = Integer
data Tree a = Leaf
    | Node Height (Tree a) a (Tree a)
    deriving (Show, Eq)

getHeight :: Tree a -> Height
getHeight Leaf = -1
getHeight (Node height _ _ _) = height

-- generate a balanced binary tree
foldTree :: [a] -> Tree a
foldTree = foldr insertIntoTree Leaf

insertIntoTree :: a -> Tree a -> Tree a
insertIntoTree a Leaf = Node 0 Leaf a Leaf
insertIntoTree a (Node height treeL val treeR)
    | getHeight treeL < getHeight treeR = Node height (insertIntoTree a treeL) val treeR
    | getHeight treeL > getHeight treeR = Node height treeL val (insertIntoTree a treeR)
    | otherwise = let newNode = (insertIntoTree a treeL)
                    in Node (getHeight newNode + 1) newNode val treeR -- equal heights 

-- xor using fold
xor :: [Bool] -> Bool
xor = foldr (\val acc -> if val then not acc else acc) False

-- implement map using fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr(\x acc -> f x : acc) []

--Given an integer n, your function should generate all the odd prime numbers up to 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : (map (\i -> 2*i + 1 ) $ difference [1..n] (sieveBuilder n))
-- sieveSundaram  n = 
--     let sieve = sieveBuilder n
--         filteredNums = filter (\i -> not $ elem i sieve) [1..n]
--     in map (\i -> (2*i) + 1) filteredNums

-- sorted list difference for 1st list
difference :: [Integer] -> [Integer] -> [Integer]
difference xs [] = xs
difference [] _ = []
difference l1@(x:xs) l2@(y:ys)
    | x < y = x : difference xs l2
    | x > y = difference l1 ys
    | otherwise = difference xs ys

sieveBuilder :: Integer -> [Integer]
sieveBuilder n = 
    let n'=fromIntegral n
        calc i j = i + j + (2 * i * j)
        jvals i = let i' = fromIntegral i in [i..floor ((n'-i')/(2*i'+1))]
    in foldl' (\acc i -> sortedMerge acc (fmap (calc i) (jvals i))) [] [1..floor (sqrt (n' / 2))]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
-- merges two sorted lists into a sorted order, removing duplicates
sortedMerge :: [Integer] -> [Integer] -> [Integer]
sortedMerge [] x = x
sortedMerge x [] = x
sortedMerge (l1@(x:xs)) (l2@(y:ys))
    | x < y = x : sortedMerge xs l2
    | x > y = y : sortedMerge l1 ys
    | otherwise = x : sortedMerge xs ys



pe_007 = last $ take 10000001 (sieveSundaram 10000001)

main :: IO ()
main = do
    print pe_007



