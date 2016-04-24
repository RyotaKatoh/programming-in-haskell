module Chap2 () where
double x = x + x
quadruple x = double (double x)

factorial n = product[1..n]
average ns = sum ns `div` length ns

a = b + c
  where
    b = 1
    c = 3

nFunc = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]

my_last ns = head (reverse ns)

my_init1 ns = take (length ns - 1) ns
my_init2 ns = reverse ( drop 1 (reverse ns) )
