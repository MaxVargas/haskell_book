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

---------------------

item :: Parser Char
item = P (\inp -> case inp of
                      []     -> []
                      (x:xs) -> [(x,xs)])

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

comment :: Parser ()
comment = do x <- item
             if x == '-' then
                do y <- item
                if y == '-' then
                   return
                else return [x,y]
             else return [x]
