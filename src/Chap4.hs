module Chap4 () where
-- 1
halve ns = (take (length ns `div` 2) ns, drop (length ns `div` 2 ) ns)

-- 2

tail01 :: [a] -> [a]
tail01 xs = if not (null xs) then tail xs else []

tail02 xs | not (null xs) = tail xs
          | otherwise = []

tail03 :: [a] -> [a]
tail03 [] = []
--tail03 xs = tail xs
tail03 (_: xs) = xs

-- 3
v1 :: Bool -> Bool -> Bool
v1 True True = True
v1 True False = True
v1 False True = True
v1 False False = False

v2 :: Bool -> Bool -> Bool
v2 False False = False
v2 _ _ = True

v3 :: Bool -> Bool -> Bool
v3 True _ = True
v3 False True = True
v3 False False = False

v4 :: Bool -> Bool -> Bool
v4 True _ = True
v4 False b = b

-- 4
and'  :: Bool -> Bool -> Bool
and' b1 b2 = if b1 == True && b2 == True then True else False

-- 5
and'' :: Bool -> Bool -> Bool
and'' b1 b2 = if b1 == True then b2 else False

-- 6
multi' = \x -> \y -> \z -> x * y * z


-- 講義


test :: [Char] -> Bool
test ['a', _, _] = True
test _ = False

hoge = \x -> \y -> x + y


--halve_hiro =
