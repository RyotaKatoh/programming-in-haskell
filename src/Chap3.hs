module Chap3 () where

second' xs = head (tail xs)

swap (x, y) = (y, x)

pair x y = (x, y)

double' x = x*2

twice f x = f (f x)
