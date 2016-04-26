module Chap9_life (glider,glidergun,life) where

import Chap9_calc

width :: Int
width = 10000000

height :: Int
height = 10000000

type Board = [Pos]

glider :: Board
glider = [(4,2), (2,3), (4,3), (3, 4), (4,4)]

glidergun :: Board
glidergun = [(2,6),(3,6),(2,7),(3,7),
             (13,6),(13,7),(13,8),(14,5),(14,9),(15,4),(15,10),(16,5),(16,9),(17,6),(17,7),(17,8),(18,6),(18,7),(18,8),
             (23,4),(23,5),(23,6),
             (24,3),(24,4),(24,6),(24,7),
             (25,3),(25,4),(25,6),(25,7),
             (26,3),(26,4),(26,5),(26,6),(26,7),
             (27,2),(27,3),(27,7),(27,8),
             (32,6),(32,7),
             (36,4),(36,5),
             (37,4),(37,5) ]

showcells ::  Board -> IO ()
showcells b = seqn [writeat p "○" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x-1, y-1), (x, y-1),
                           (x+1, y-1), (x+1, y),
                           (x+1, y+1), (x, y+1),
                           (x-1, y+1), (x-1, y)]

wrap :: Pos -> Pos
wrap (x, y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs


survivors :: Board -> [Pos]
survivors b = [p | p <- b, liveneighbs b p `elem` [2, 3]]

-- births :: Board -> [Pos]
-- births b = [(x, y) | x <- [1..width], y <- [1..height],  isEmpty b (x, y), liveneighbs b (x, y) == 3]

births b = [p | p <- rmdups (concat (map neighbs b)), isEmpty b p, liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board  -> IO ()
life b = do cls
            showcells b
            wait 50000
            life (nextgen b)

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]
