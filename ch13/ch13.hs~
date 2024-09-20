import Control.Applicative
import Data.Char
import System.IO

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

----------------------
-- Examples of parsers
item :: Parser Char
item = P (\inp -> case inp of
                      []     -> []
                      (x:xs) -> [(x,xs)])

succer :: Enum a => Parser (a->a)
succer = pure succ

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
  where
    g x y z = (x,z)

{- alt def of three -}
three' :: Parser (Char, Char)
three' = do x <- item
            _ <- item
            z <- item
            return (x,z)

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
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident =
  do x  <- lower
     xs <- many alphanum
     return (x:xs)

nat :: Parser Int
nat =
  do xs <- some digit
     return (read xs)

space :: Parser ()
space = 
  do many (sat isSpace)
     return ()

int :: Parser Int
int =
  do char '-'
     n <- nat
     return (-n)
  <|> nat

token :: Parser a -> Parser a
token p = 
  do _ <- space
     x <- p
     _ <- space
     return x

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats =
  do _ <- symbol "["
     n <- natural
     ns<- many (do symbol ","
                   natural)
     _ <- symbol "]"
     return (n:ns)
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
-- Arithmetic

expr :: Parser Int
expr =
  do t <- term
     do symbol "+"
        e <- expr
        return (t+e)
      <|> do symbol "-"
             e <- expr
             return (t-e)
      <|> return t

term :: Parser Int
term =
  do f <- factor
     do symbol "*"
        t <- term
        return (f*t)
      <|> do symbol "/"
             t <- term
             return (f `div` t) 
      <|> return f

factor :: Parser Int
factor =
  do symbol "("
     e <- expr
     symbol ")"
     return e
  <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
  [(n,[])] -> n
  [(_,out)]-> error ("Unused input " ++ out)
  []       -> error "Invalid input"

----------------------
--Calculator

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs =
  do goto p
     putStr xs

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x,y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

getCh :: IO Char
getCh = 
  do hSetEcho stdin False
     x <- getChar
     hSetEcho stdin True
     return x

box :: [String]
box = ["+----------------",
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

buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]

display xs = 
  do writeat (3,2) (replicate 13 ' ')
     writeat (3,2) (reverse (take 13 (reverse xs)))

beep :: IO ()
beep = putStr "\BEL"

calc :: String -> IO ()
calc xs = 
  do display xs
     c <- getCh
     if elem c buttons then
        process c xs
     else
        do beep
           calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"    = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n"       = evalu xs
             | elem c "cC"        = clear
             | otherwise          = press c xs

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

evalu :: String -> IO ()
evalu xs = case parse expr xs of
               [(n,[])] -> calc (show n)
               _        -> do beep
                              calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear
