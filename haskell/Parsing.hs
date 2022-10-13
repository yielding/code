module Parsing where

import Data.Char
import Control.Monad
import Control.Applicative hiding (many)


-- Sequencing parsers 

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  -- fmap g p = P (\inp -> case parse p inp of 
  --                         []         -> []
  --                         [(v, out)] -> [(g v, out)]) 

  fmap = liftM

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\input -> [(v, input)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of 
                            []         -> []
                            [(g, out)] -> parse (fmap g px) out) 

instance Monad Parser where
  return v = P (\inp -> [(v, inp)])
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P(\inp -> case parse p inp of 
                          [] -> []
                          [(v, out)] -> parse (f v) out)

instance MonadPlus Parser where
   mzero        =  P (\inp -> [])
   p `mplus` q  =  P (\inp -> case parse p inp of
                                 []        -> parse q inp
                                 [(v,out)] -> [(v,out)])

-- Making choices

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

  --p <|> q = P (\inp -> case parse p inp of 
  --                        []         -> parse q inp
  --                        [(v, out)] -> [(v, out)])

-- Basic parsers
-- https://stackoverflow.com/questions/31815310/the-difference-between-type-and-newtype-in-haskell
-- P :: (P :: (String -> [(a, String)]) -> Parser a

newtype Parser a = P (String -> [(a, String)])

failure :: Parser a
failure = mzero

item :: Parser Char
item = P (\input -> case input of 
                      []     -> []
                      (x:xs) -> [(x, xs)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input

-- Choice
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

-- Derived privitives
sat :: (Char -> Bool) -> Parser Char 
sat p = do x <- item
           if p x then return x else empty 

digit :: Parser Char 
digit = sat isDigit

lower :: Parser Char 
lower = sat isLower

upper :: Parser Char 
upper = sat isUpper

letter :: Parser Char 
letter = sat isAlpha 

alphanum :: Parser Char 
alphanum = sat isAlphaNum 

char :: Char -> Parser Char 
char x = sat (== x)

string :: String -> Parser String 
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

many :: Parser a -> Parser [a]
many p =  many1 p +++ return []

-- Apply the parser at least one time
many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int 
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      +++ nat

space :: Parser ()
space = do many (sat isSpace)  
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space 
             return v

identifier :: Parser String
identifier = token ident 

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

ppp :: Parser [Int]
ppp = do symbol "["
         n  <- natural
         ns <- many (do symbol ","; natural)
         symbol "]"
         return (n:ns)

-- main = do 
--   let r0 = parse ppp "   [1, 2, 3]"
--   let r1 = fst (head r0)
--   let r2 = head $ fst (head r0)
-- 
--   print(r0)
--   print(r1)
--   print(r2)
