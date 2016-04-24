-- this is copy & paste from https://github.com/nozaq/programming-in-haskell/blob/master/src/Ch8.hs
-- Thank you nozaq
module Chap8 (
  parse, integer, comment, expr, expr2, expr3, expr4,
) where

import Data.Char
import Control.Applicative
import Control.Monad

infixr 5 +++

-- Parser implementation taken from the book's website.
-- http://www.cs.nott.ac.uk/~pszgmh/Parsing.lhs
newtype Parser a              =  P (String -> [(a,String)])

instance Functor Parser where
   fmap  = liftM

instance Applicative Parser where
   pure v = P (\inp -> [(v,inp)])
   (<*>) = ap

instance Alternative Parser where
   empty = P (\inp -> [])
   (<|>) p q = P (\inp -> case parse p inp of
                            []        -> parse q inp
                            [(v,out)] -> [(v,out)])

instance Monad Parser where
   return = pure
   p >>= f = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

(+++)                         :: Parser a -> Parser a -> Parser a
p +++ q                       =  p `mplus` q

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

manyn                          :: Parser a -> Parser [a]
manyn p                        =  many1 p +++ return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- manyn p
                                    return (v:vs)

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- manyn alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

space                         :: Parser ()
space                         =  do manyn (sat isSpace)
                                    return ()

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat


symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)

-- 1.
int :: Parser Int
int =  do char '-'
          n <- nat
          return (-n)
       +++ nat

integer :: Parser Int
integer =  token int

-- 2.
isNotNewLine :: Char -> Bool
isNotNewLine c = c /= '\n'

comment :: Parser ()
comment = do string "--"
             manyn (sat isNotNewLine)
             char '\n'
             return ()

-- 3.
{-
1. + 2 + 3 4
2. + + 2 3 4
-}

-- 4.
{-
1. + 2 3
2. * 2 * 3 4
3. + + 2 3 4
-}

-- 5.
{-
It's equivalent to cache the intermediate parsing result instead of re-parsing the same part if the first matching fails.
-}

-- 6.
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t+e)
           +++ do symbol "-"
                  e <- expr
                  return (t-e)
           +++ return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f*t)
           +++ do symbol "/"
                  t <- term
                  return (f `div` t)
           +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
           +++ natural

-- 7.
expr2 :: Parser Int
expr2 = do t <- term2
           do symbol "+"
              e <- expr2
              return (t+e)
            +++ do symbol "-"
                   e <- expr2
                   return (t-e)
            +++ return t

term2 :: Parser Int
term2 = do f <- pow
           do symbol "*"
              t <- term2
              return (f*t)
            +++ do symbol "/"
                   t <- term2
                   return (f `div` t)
            +++ return f

pow :: Parser Int
pow = do f <- factor2
         do symbol "^"
            t <- pow
            return (f^t)
          +++ return f

factor2 :: Parser Int
factor2 = do symbol "("
             e <- expr
             symbol ")"
             return e
            +++ natural

-- 8.
-- 8.a.
{-
expr ::= expr '-' nat | nat
nat  ::= '0' | '1' | '2' | ...
-}

-- 8.b.
expr3 :: Parser Int
expr3 = do l <- expr3
           symbol "-"
           r <- nat
           return (l-r)
         +++ nat

-- 8.c.
-- This parser never stops.

-- 8.d.
expr4 :: Parser Int
expr4 = do l <- natural
           rs <- many (do symbol "-"
                          natural)
           return (foldl (-) l rs)
