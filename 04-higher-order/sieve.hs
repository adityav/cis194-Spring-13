{-# OPTIONS_GHC -O2 #-}

marked :: [Int]
marked = fold $ map mark [1..]
    where
        fold ((x:xs):t) = x : (xs `union` fold t)
        mark i = map (calc i) [i..]
        calc i j = i + j + 2*i*j

union :: [Int] -> [Int] -> [Int]
union a         []        = a
union []        b         = b
union (x:xs) (y:ys)
    | x < y     = x : union xs (y:ys)
    | x == y    = x : union xs ys
    | otherwise = y : union (x:xs) ys

removeComposites :: [Int] -> [Int] -> [Int]
removeComposites [] _          = []
removeComposites (s:ss) []     = 2*s + 1 : removeComposites ss []
removeComposites (s:ss) (c:cs)
    | s == c    = removeComposites ss cs
    | s > c     = removeComposites (s:ss) cs
    | otherwise = 2*s + 1 : removeComposites ss (c:cs)

sieveSundaram :: [Int] 
sieveSundaram = 2:(removeComposites [1..] marked)

pe_007 = last $ take n sieveSundaram
    where n = 10000001

main :: IO ()
main = do
    print pe_007