module Chap7 () where

import Data.Char

myMap :: (a->b) -> [a] -> [b]
myMap f [] = []
myMap f (x: xs) = f x : (myMap f xs)

myFilter :: (a->Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f xs = [x | x <- xs, f x]

squareSum :: [Int] -> Int
squareSum ns = sum( myMap (^2) ns )

-- 7.6
type Bit = Int

-- bin2int :: [Bit] -> Int
-- bin2int bits = sum [w*b | (w, b) <- zip weights bits]
--                where weights = iterate (*2) 1
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bit :: Int -> [Bit]
int2bit 0 = []
int2bit n = n `mod` 2 : int2bit (n `div` 2)

take8 :: [Bit] -> [Bit]
take8 bit = take 8 (bit ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (take8 . int2bit . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map(chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

-- 1.
myQ1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
myQ1 f p xs  = map f ( filter p xs )

-- 2.

myAll :: (a -> Bool) -> [a] -> Bool
myAll p = and . map p


myAny :: (a -> Bool) -> [a] -> Bool
myAny p = or . map p

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p [] = []
myDropWhile p (x: xs) | p x = myTakeWhile p xs
                      |otherwise = x:xs

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p (x:xs) | p x = x : myTakeWhile p xs
                     | otherwise = []


-- 3.
myMap' :: (a -> b) -> [a] -> [b]
myMap' f = foldr (\x xs -> f x : xs) []

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' p = foldr (\x xs -> if p x then x:xs else xs) []


-- 4.
dec2int :: [Int] -> Int
dec2int = foldl (\n ns -> n*10 + ns) 0
