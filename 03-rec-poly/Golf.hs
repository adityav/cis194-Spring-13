{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.Maybe

skips :: [a] -> [[a]]
skips [] = []
skips xs = map (\index -> pluck index xs) (enumFromTo 1 (length xs))


-- takes every nth elem from the list
pluck :: Int -> [a] -> [a]
pluck index xs
    | index > length xs = []
    | otherwise = 
        let indices = tail $ enumFromThenTo 0 index  (length xs) -- indices to pick. tail to remove 0
        in foldr (\x acc -> (xs !! (x-1)) : acc) [] indices


localMaxima :: [Integer] -> [Integer]
localMaxima xs
    | length xs < 3 = []
    | otherwise = catMaybes $ zipWith3 maxima xs (tail xs) (tail $ tail xs)

-- returns b if it is maximum among 3 othwise nothing
maxima :: Integer -> Integer -> Integer -> Maybe Integer
maxima a b c = if a < b && b > c then Just b else Nothing

-- list contains 0-9
histogram :: [Integer] -> String
-- histogram :: [Integer] -> [String]
histogram xs =
    let counts = countNaturals xs
        maxVal = maximum counts
        starLines = fmap (genLine counts) [1..maxVal]
    in (unlines (reverse starLines)) ++ "==========\n0123456789\n"

-- generates a starline
genLine :: [Int] -> Int -> String
genLine counts baseVal  = fmap (\n -> if baseVal <= n then '*' else ' ' ) counts

-- counts to number of occurances of numbers 0..9
countNaturals :: [Integer] -> [Int]
countNaturals xs = map (\n -> length $ filter (==n) xs) [0..9]