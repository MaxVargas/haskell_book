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

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty


char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

nextline :: Parser ()
nextline = do x <- item
              if '\n'==x then return () else nextline

-- this method does not include the \n on the return
comment :: Parser ()
comment = do x <- string "--"
             nextline

{- alternate definition with many
 - this method *includes* the \n on the return
comment' = do x <- string "--"
              many (sat (/= '\n'))
              return ()
-}

-- 2) This problem is drawing trees. It's.... drawn on paper
-- 3) same here
-- 4) This problem deals with the grammar simplification on p.187-189
--    The final simplification has a difference of:
--    e := t+e | t              e := t(+e | id)
--    t := f*t | f      vs      t := f(*t | id)
--    The left side forces a recalculation of terms if we ever land in the second case
--        and a recalculation of factors whenever we land in the second case too
--    The right side keeps track of terms we've already calculated
--        so that we don't have to recalculate them. Kind of like a tree
--        based memoization.
