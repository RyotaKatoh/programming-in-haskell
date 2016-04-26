module Chap9 (readLine, playNim) where

import Chap9_calc
import Data.Char
-- 1.

readLine = get ""
get xs = do x <- getChar
            case x of
              '\n' -> return xs
              '\DEL' -> if null xs then
                          get xs
                        else
                          do putStr "\ESC[1D \ESC[1D"
                             get (init xs)
              _ -> get (xs ++ [x])


-- 2.
type Nim = [Int]

initial :: Nim
initial = [5,4,3,2,1]

finished :: Nim -> Bool
finished n = all (== 0) n

valid :: Nim -> Int -> Int -> Bool
valid n row num = n !! (row - 1) >= num

move :: Nim -> Int -> Int -> Nim
move nim row num = [if r == row then n - num else n | (r, n) <- zip [1..5] nim]

newline :: IO ()
newline = putChar '\n'

putNim :: Nim -> IO ()
putNim [a,b,c,d,e] = do putRow 1 a
                        putRow 2 b
                        putRow 3 c
                        putRow 4 d
                        putRow 5 e

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (stars num)

stars :: Int -> String
stars n = concat (replicate n "* ")

getDigit :: String -> IO Int
getDigit prom = do putStr prom
                   x <- getChar
                   newline
                   if isDigit x then
                     return (ord x - ord '0')
                   else
                     do putStrLn "ERROR: Invalid digit"
                        getDigit prom

next :: Int -> Int
next 1 = 2
next 2 = 1

playNim :: IO ()
playNim = play initial 1

play :: Nim -> Int -> IO ()
play nim player = do newline
                     putNim nim
                     if finished nim then
                       do newline
                          putStr "Player "
                          putStr (show (next player))
                          putStrLn " wins!!"
                     else
                       do newline
                          putStr "Player "
                          putStrLn (show player)
                          r <- getDigit "Enter a row number: "
                          n <- getDigit "Starts to remove: "
                          if valid nim r n then
                            play (move nim r n) (next player)
                          else
                            do newline
                               putStrLn "ERROR: Invalid move"
                               play nim player


-- showNim :: Nim -> IO ()
-- showNim ns = seqn [writeat (1, y) (show y ++ ": " ++  replicate n '*' ) | (y, n) <- zip [1..(length ns)] ns]
--
-- inputTarget :: (Int, Int) -> IO()
--
-- deleteNim :: (Int, Int) -> Nim
-- deleteNim (row, n) =
