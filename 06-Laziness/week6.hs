{-# OPTIONS_GHC -Wall #-}

import Data.List

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- infinite fibs
fib2 :: [Integer]
-- fib2 0 = [1]
-- fib2 1 = [1,1]
fib2 = 1:1 : zipWith (+) fib2 (tail fib2)