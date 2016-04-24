module Chap5 () where

import Data.Char

myConcat :: [[a]] -> [a]
myConcat xss = [x | xs <- xss, x<- xs]

myFirst ps = [x | (x, _) <- ps]

myFactor :: Int -> [Int]
myFactor n = [x | x <- [1..n], n `mod` x == 0]

myPrime :: Int -> Bool
myPrime n = myFactor n == [1, n]

myPrimes :: Int -> [Int]
myPrimes n = [x | x<-[1..n], myPrime x]

myFind :: Eq a => a -> [(a, b)] -> [b]
myFind k t = [v | (k', v) <- t, k==k' ]

pair :: [a] -> [(a, a)]
pair xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pair xs]

position :: Eq a => a -> [a] -> [Int]
position x xs = [i | (x', i) <- zip xs [0..n], x' == x]
                where n = length xs -1

lowers :: String -> Int
lowers s = length [c | c<-s, isLower c]

count :: Char -> String -> Int
count c s = length [c' | c' <- s , c' == c]


-- Ceasar encription
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

ulet2int :: Char -> Int
ulet2int c = ord c - ord 'A'

int2ulet :: Int -> Char
int2ulet n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2ulet ((ulet2int c + n) `mod` 26 )
          | otherwise = c

encode :: Int -> String -> String
encode n s = [shift n x | x <- s]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) *100

freq :: String -> [Float]
freq xs = [percent (count x xs) n | x <- ['a'..'z']]
          where n = lowers xs

table :: [Float]
table =  [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum[ ( (o - e) ^2 )/e | (o, e) <- zip os es ]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (position (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n<-[0..25]]
    table' = freq xs


-- Exercise 5

-- 1.

squaresum :: Int -> Int
squaresum n = sum[x^2 | x <- [1..n]]

-- 2.
myReplicate'' :: Int -> a -> [a]
myReplicate'' n x = [x | _ <- [1..n] ]

-- 3.
-- to be discussed.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z)| x<-[1..n], y<-[1..n], z<-[1..n], x^2+y^2==z^2]

-- 4.
perfects :: Int -> [Int]
perfects n = [x | x<-[1..n], sum( drop 1 (reverse( myFactor x) )) == x]

-- 5.
gen2 :: [a] -> [a] -> [(a, a)]
gen2 a b = concat[[(x, y) | y<-b]| x<-a]

-- 6.
positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = myFind x (zip xs [0..n])
  where
    n = length xs

-- 7.
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum[ x*y | (x, y) <-zip xs ys ]

-- 8.
toLowers :: String -> String
toLowers s = [toLower c | c <- s]

crack2 :: String -> String
crack2 xs = encode (-factor) (toLowers xs)
  where
    factor = head (position (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n<-[0..25]]
    table' = freq (toLowers xs)


-- debug
count' :: Char -> String -> Int
count' x xs = sum[ 1 | x' <- xs, x == x']
