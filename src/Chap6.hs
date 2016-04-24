module Chap6 () where
-- 1

pow :: Int -> Int -> Int
n1 `pow` 0 = 1
n1 `pow` n2 = n1 * (n1 `pow` (n2 -1))

-- 2
myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

myDrop :: Int -> [a] -> [a]
myDrop 0 [] = []
myDrop 0 (x: xs) = x:xs
myDrop n [] = []
myDrop n (x : xs) = myDrop (n-1) xs

myInit :: [a] -> [a]
myInit [_] = []
myInit (x : xs) = x : myInit xs

-- 3
myAnd :: [Bool] -> Bool
myAnd [b] = b
myAnd (b : bs) | b  = myAnd bs
               | otherwise = False

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x : xs) = x ++ myConcat xs

myReplicate :: Int -> a -> [a]
myReplicate 0 x = []
myReplicate n x = (x : myReplicate (n-1) x)

myBikkuri :: [a] -> Int -> a
myBikkuri (x : xs) 0 = x
myBikkuri (x : xs) n = myBikkuri xs (n-1)

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x [x'] | x == x' = True
              | otherwise = False
myElem x (x' : xs) | x == x' = True
                   | otherwise = myElem x xs

-- 4
merge :: Ord a =>  [a] -> [a] -> [a]
merge [] [] = []
merge x [] = x
merge [] x = x
merge (a:as) (b:bs) | a <= b = a :  merge as (b:bs)
                    | otherwise = b : merge (a:as) bs

-- 5
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort lx) (msort rx)
  where (lx, rx) = halve xs

-- 6
mySum :: Num a=> [a] -> a
mySum [] = 0
mySum (n: ns) = n + mySum(ns)

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n [] = []
myTake n (x : xs) = x : myTake (n-1) xs

myLast :: [a] -> a
myLast [x] = x
myLast (x : xs) = myLast xs
