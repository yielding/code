-- https://github.com/changgyhub/haskell-calculator/blob/master/src/Calculator.hs
import Parsing
import System.IO
import Data.Char (chr)

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Sqr Expr 
          | Neg Expr 
          | Val Int deriving (Show)

evalExpr :: Expr -> Maybe Int
evalExpr (Add e1 e2) = case (evalExpr e1, evalExpr e2) of
                        (Just x, Just y) -> Just (x + y)
                        _                -> Nothing
evalExpr (Sub e1 e2) = case (evalExpr e1, evalExpr e2) of
                        (Just x, Just y) -> Just (x - y)
                        _                -> Nothing
evalExpr (Mul e1 e2) = case (evalExpr e1, evalExpr e2) of
                        (Just x, Just y) -> Just (x * y)
                        _                -> Nothing
evalExpr (Div e1 e2) = case (evalExpr e1, evalExpr e2) of
                        (Just x, Just 0) -> Nothing
                        (Just x, Just y) -> Just (x `div` y)
                        _                -> Nothing
evalExpr (Mod e1 e2) = case (evalExpr e1, evalExpr e2) of
                        (Just x, Just 0) -> Nothing
                        (Just x, Just y) -> Just (x `mod` y)
                        _                -> Nothing
evalExpr (Sqr e) = case evalExpr e of
                        Just x -> Just (x * x)
                        _      -> Nothing
evalExpr (Neg e) = case evalExpr e of
                        Just x -> Just (negate x)
                        _      -> Nothing

evalExpr (Val x) = Just x

-- expr -> term op_term
-- op_term -> ε | “+” term op_term | “-” term op_term
-- term -> factor op_factor
-- op_factor -> ε | “*” factor op_factor | “/” factor op_factor | “%” factor op_factor
-- factor -> “sqr(” expr “)” | “neg(” expr “)” | “(” expr “)” | nat

expr :: Int -> Parser Expr
expr n = 
    do t <- term n
       o <- op_term n t
       return o

op_term :: Int -> Expr -> Parser Expr
op_term n t1 =
    do symbol "+"
       t2 <- term n
       o  <- op_term n (Add t1 t2)
       return o
    +++ do symbol "-"
           t2 <- term n
           o  <- op_term n (Sub t1 t2)
           return o
    +++ return t1

term :: Int -> Parser Expr
term n = do f <- factor n
            o <- op_factor n f
            return o

op_factor :: Int -> Expr -> Parser Expr
op_factor n f1 = 
    do symbol "*"
       f2 <- factor n
       o <- op_factor n (Mul f1 f2)
       return o
    +++ 
    do symbol "/"
       f2 <- factor n
       o <- op_factor n (Div f1 f2)
       return o
    +++ 
    do symbol "%"
       f2 <- factor n
       o <- op_factor n (Mod f1 f2)
       return o
    +++ 
    return f1

factor :: Int -> Parser Expr
factor n = 
    do symbol "sqr("
       e <- expr n
       symbol ")"
       return (Sqr e)
    +++ 
    do symbol "neg("
       e <- expr n
       symbol ")"
       return (Neg e)
    +++ 
    do symbol "("
       e <- expr n
       symbol ")"
       return e
    +++ 
    do num <- if n == 0 then nat else cat
       return (Val num)

cat :: Parser Int
cat = do xs <- many1 alphanum
         return (hexToDec xs)

type Pos = (Int, Int)

-- Set the cursor at position (x, y).
goto :: Pos -> IO ()
goto (x, y) = putStr $ "\ESC[" ++ show x ++ ";" ++ show y ++ "H"

-- Write text xs at position p.
writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

-- Sequencing.
seqn :: [IO a] -> IO ()
seqn []     = return ()
seqn (a:as) = do a
                 seqn as

-- Erase the screen.
cls :: IO ()
cls = putStr "\ESC[2J"

-- Read a single character from keyboard, without echoing to the screen.
getCh :: IO Char
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

box :: [String]
box = ["+-DEC-----------+",
       "|               |",
       "|               |",
       "+---+---+---+---+",
       "| Q | C | D | M |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+",
       "| % |sqr|neg| = |",
       "+---+---+---+---+"]

boxWidth :: Int
boxWidth = 17

boxHeight :: Int
boxHeight = 16 

buttons :: String
buttons = "+-*/%=()QCD" ++ map chr ([48..57] ++ [97..122])

showBox :: IO ()
showBox = seqn [writeAt (y, 1) xs | (y, xs) <- zip [1..boxHeight] box]

main = do cls
          showBox
          calc 0 ""

calc :: Int -> String -> IO ()
calc n xs = do displayExpr xs
               c <- getCh
               process n c xs

-- Problem 3 Part I: TODO
displayExpr :: String -> IO ()
displayExpr xs = do writeAt (2,2) "               "
                    if length xs < boxWidth - 2
                       then writeAt (2,2) $ replicate (boxWidth - length xs - 2) ' ' ++ xs
                       else writeAt (2,2) $ ">" ++ drop (length xs - boxWidth + 3) xs

-- Problem 3 Part II: TODO
process :: Int -> Char -> String -> IO ()
process n c xs
  | c == 'Q' = goto (boxHeight + 1, 1)                                       -- quit
  | c == 'D' = (if xs == "" then calc n "" else calc n (init xs))            -- delete
  | c == 'C' = calc n ""                                                     -- clear
  | c == '=' = eval n xs                                                     -- eval
  | c == 'M' = do if n == 0 then writeAt(1,3) "HEX" else writeAt(1,3) "DEC"
                  if n == 0 then calc 1 xs else calc 0 xs                    -- change mode
  | elem c buttons = calc n (xs ++ [c])                                      -- press
  | otherwise = calc n xs

-- Problem 3 Part III: TODO
eval :: Int -> String -> IO ()
eval n xs =  case parse (expr n) xs of
           [(e,"")] -> case evalExpr e of 
                        (Just a) -> if n == 0 then displayAns n xs (show a) else displayAns n xs (decToHex a)
                        Nothing -> displayAns n xs "NaN"
           _        -> displayAns n xs "ERROR"

displayAns :: Int -> String -> String -> IO ()
displayAns n pxs xs = do writeAt (3,2) "               "
                         if length xs < boxWidth - 2
                          then writeAt (3,2) $ replicate (boxWidth - length xs - 2) ' ' ++ xs
                          else writeAt (3,2) $ ">" ++ drop (length xs - boxWidth + 3) xs
                         c <- getCh
                         process n c pxs


decToHex :: Int -> String
decToHex 0 = []
decToHex n = if (n >= 0 ) then decToHex (n `div` 16) ++ [hexChar $ n `mod` 16] else '-' : decToHex (-n)

hexToDec :: String -> Int
hexToDec [] = 0
hexToDec (x:xs) =  if (x == '-' ) then -hexToDec xs else 16 ^ length xs * hexInt x + hexToDec xs

hexChar :: Int -> Char
hexChar 1 = '1'
hexChar 2 = '2'
hexChar 3 = '3'
hexChar 4 = '4'
hexChar 5 = '5'
hexChar 6 = '6'
hexChar 7 = '7'
hexChar 8 = '8'
hexChar 9 = '9'
hexChar 10 = 'a'
hexChar 11 = 'b'
hexChar 12 = 'c'
hexChar 13 = 'd'
hexChar 14 = 'e'
hexChar 15 = 'f'
hexChar _ = '0'


hexInt :: Char -> Int
hexInt '1' = 1
hexInt '2' = 2
hexInt '3' = 3
hexInt '4' = 4
hexInt '5' = 5
hexInt '6' = 6
hexInt '7' = 7
hexInt '8' = 8
hexInt '9' = 9
hexInt 'a' = 10
hexInt 'b' = 11
hexInt 'c' = 12
hexInt 'd' = 13
hexInt 'e' = 14
hexInt 'f' = 15
hexInt _ = 0