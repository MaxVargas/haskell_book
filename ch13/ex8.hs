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

-----------------------------------
-- 8)
-- A grammar for subtraction, associating to the left
-- expr ::=  expr - nat | nat
-- nat  ::= 0 | 1 | 2 | ...
--
-- examples:
-- 4 - (3 - 6) - 2 ==> (4 - (3 - 6)) - 2
-- 4 - 7 - 8 - 9   ==> ((4 - 7) - 8) - 9
--
-- b) There's a lot of ways to write an incorrect parser hahaha
--
-- c) Some of the problems include:
-- "getting to the right"
-- we read strings from left to right... but in order to
-- associte to the left, we need to process the right of
-- of the string before the left.
--
-- associating left does solve a different problem you
-- run into when trying to associate right, which is keep
-- track of the negative symbols..
--
-- There's also the point that naturals are not closed
-- under subtraction, but that's moreso an issue with the
-- grammar.


expr :: Parser Int
expr =
  do t <- term
     do _ <- symbol "-"
        return t
      <|> return t

exprs :: Parser Int
exprs =
  do es <- many expr
     return (foldl (-) (head es) (tail es))

term :: Parser Int
term =
  do _  <- symbol "("
     es <- exprs
     _  <- symbol ")"
     return es
   <|> natural
     
eval :: String -> Int
eval xs = case (parse exprs xs) of
  [(n,[])] -> n
  [(_,out)]-> error ("Unused input " ++ out)
  []       -> error "Invalid input"
   

     
