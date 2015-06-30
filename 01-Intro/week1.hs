
-- validate credit card nums
double2ndDigit :: Num a => [a] -> [a]
double2ndDigit [] = []
double2ndDigit (x:[]) = x : []
double2ndDigit (x:y:xs) = x : y*2 : double2ndDigit xs

step1 :: Num a => [a] -> [a]
step1 xs = reverse $ double2ndDigit $ reverse xs 

isValidCC :: [Int] -> Bool
isValidCC xs = (sum (step1 xs) `div` 10) == 0

-- Ex1 find digits of number
toDigits :: Integer -> [Integer]
toDigits num = reverse (toDigitsRev num)

toDigitsRev :: Integer -> [Integer]
toDigitsRev num
    | num < 10 = num : []
    | otherwise = num `rem` 10 : toDigitsRev (num `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = x : []
doubleEveryOther (x:xs)
    | even (length xs) = x : doubleEveryOther xs
    | otherwise = x*2 : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits xs

-- ex2 Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
-- num of disks, move from peg a to b via c
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n == 1 = [(a,b)]
    | otherwise = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)