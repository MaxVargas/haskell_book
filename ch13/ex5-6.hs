import Control.Applicative
import Data.Char
import System.IO

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

---------------------

prodMap :: (a->c) -> (b->d) -> (a,b) -> (c,d)
prodMap f g = (\(x,y) -> (f x, g y))

instance Functor Parser where
  -- fmap :: (a->b) -> Parser a -> Parser b
  fmap f p = P( map (prodMap f id) . (parse p) )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\inp -> [(x, inp)])

  -- (<*>) :: Parser (a->b) -> Parser a -> Parser b
  pf <*> px = P (\inp -> case parse pf inp of
                            [] -> []
                            [(f,out)] -> parse (fmap f px) out)

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  px >>= f = P (\inp ->
     case parse px inp of
          []         -> []
          [(x, out)] -> parse (f x) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = 
    P(\inp -> case parse p inp of
              []         -> parse q inp
              [(x, out)] -> [(x, out)])

----------------------------------
-- Support functions
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

item :: Parser Char
item = P (\inp -> case inp of
                      []     -> []
                      (x:xs) -> [(x,xs)])

space :: Parser ()
space = 
  do many (sat isSpace)
     return ()

token :: Parser a -> Parser a
token p = 
  do _ <- space
     x <- p
     _ <- space
     return x

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)
nat :: Parser Int
nat =
  do xs <- some digit
     return (read xs)

digit :: Parser Char
digit = sat isDigit

int :: Parser Int
int =
  do char '-'
     n <- nat
     return (-n)
  <|> nat

integer :: Parser Int
integer = token int

ident :: Parser String
ident =
  do x  <- lower
     xs <- many alphanum
     return (x:xs)

identifier :: Parser String
identifier = token ident

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

alphanum :: Parser Char
alphanum = sat isAlphaNum

-----------------------------------
-- 5)
-- Arithmetic
data Expr = 
  Val Int 
  | Var String 
  | Add Expr Expr 
  | Sub Expr Expr 
  | Mul Expr Expr 
  | Div Expr Expr
  deriving Show

expr :: Parser Expr
expr =
  do t <- term
     do _ <- symbol "+"
        e <- expr
        return (Add t e)
      <|> do _ <- symbol "-"
             e <- expr
             return (Sub t e)
      <|> return t

term :: Parser Expr
term =
  do f <- factor
     do _ <- symbol "*"
        t <- term
        return (Mul f t)
      <|> do _ <- symbol "/"
             t <- term
             return (Div f t)
      <|> return f

factor :: Parser Expr
factor =
  do _ <- symbol "("
     e <- expr
     _ <- symbol ")"
     return e
  <|> do x <- identifier
         return (Var x)
  <|> do n <- integer
         return (Val n)

eval :: String -> Expr
eval xs = case (parse expr xs) of
  [(n,[])] -> n
  [(_,out)]-> error ("Unused input " ++ out)
  []       -> error "Invalid input"

----------------------
-- 6) Implemented in the main file 'ch13.hs'
--    in order to complete the calculator functions
