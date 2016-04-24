module Chap9_calc (
  beep, cls, goto, Pos, writeat, seqn, showbox, display, run,
) where

import Chap8

beep :: IO()
beep = putStr "\BEL"

cls :: IO()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO()
goto (x, y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO()
seqn[] = return()
seqn (a: ax) = do a
                  seqn ax

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

showbox :: IO()
showbox = seqn [writeat (1, y) xs | (y, xs) <- zip [1..13] box]

display :: String -> IO()
display xs = do writeat (3, 2) "             "
                writeat (3, 2) (reverse (take 13 (reverse xs)))

buttons :: String
buttons = standard ++ extra
          where
            standard = "qcd=123+456-789*0()/"
            extra = "QCD \ESC\BS\DEL\n"

calc :: String -> IO ()
calc xs = do display xs
             c <- getChar
             if c `elem` buttons then
               process c xs
             else
               do beep
                  calc xs

process :: Char -> String -> IO ()
process c xs
        | c `elem` "qQ\ESC"     = quit
        | c `elem` "dD\BS\DEL"  = delete xs
        | c `elem` "=\n"        = eval xs
        | c `elem` "cC"         = clear
        | otherwise             = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
              [(n, "")] -> calc (show n)
              _ -> do beep
                      calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear


-- calc :: String -> IO()
-- calc xs = do display xs
--              c <- getChar
--              if elem c buttons

 -- buttons :: [Char]
 -- buttons = standart ++ extra
 --           where
 --             standard = "qcd=123+456-789*0()/"
 --             extra = "QCD \SEC\BS\DEL\n"
