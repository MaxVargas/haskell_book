import System.IO
import Data.Char

-- Some basics
--
act :: IO (Char, Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)

getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                return []
              else
                do xs <- getLine'
                   return (x:xs)

strlen :: IO ()
strlen =
  do putStr "Enter a string: "
     xs <- getLine
     putStr "The string has "
     putStr (show (length xs))
     putStrLn " characters"

-- Extended examples below
--
-- Hangman

hangman :: IO ()
hangman = 
  do putStrLn "Think of a word: "
     word <- sgetLine
     putStrLn "Try to guess it: "
     play word

sgetLine :: IO String
sgetLine =
  do x <- getCh
     if x == '\n' then
        do putChar x
           return []
     else
        do putChar '-'
           xs <- sgetLine
           return (x:xs)

getCh :: IO Char
getCh = 
  do hSetEcho stdin False
     x <- getChar
     hSetEcho stdin True
     return x

play :: String -> IO ()
play word =
  do putStr "? "
     guess <- getLine
     if guess == word then
        putStrLn "You got it!!"
     else
        do putStrLn (match word guess)
           play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

-- Nim

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Int -> Board
initial n = [ n-m | m <-[0..(n-1)]]

finished :: Board -> Bool
finished = all (==0)

valid :: Board -> Int -> Int -> Bool
valid board row take = board!!(row-1) >= take

move :: Board -> Int -> Int -> Board
move board row take = [update r row take | r <- zip board [1..] ]
  where
    update (r1, r2) row take = if row == r2 then r1-take else r1

putRow :: Int -> Int -> IO ()
putRow row num = 
  do putStr (show row)
     putStr ": "
     putStrLn (concat (replicate num "*"))

-- Generalization of putBoard from the book.. this was exercise 2 apparently..
putBoard' :: Board -> Int -> IO ()
putBoard' (x:xs) row =
  do putRow row x
     if length xs == 0 then putStrLn "" else putBoard' xs (row+1)

putBoard :: Board -> IO ()
putBoard xs = putBoard' xs 1

newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = 
  do putStr prompt
     x <- getChar
     newline
     if isDigit x then
        return (digitToInt x)
     else
        do putStrLn "ERROR: Invalid digit"
           getDigit prompt

playNim :: Board -> Int -> IO ()
playNim board player = 
  do newline
     putBoard board
     if finished board then
        do newline
           putStr "Player "
           putStr (show (next player))
           putStrLn " wins!"
     else
        do newline
           putStrLn "Player "
           putStrLn (show player)
           row <- getDigit "Enter a row number: "
           num <- getDigit "Stars to remove: "
           if valid board row num then
              playNim (move board row num) (next player)
           else
              do newline
                 putStrLn "Error: Invalid move"
                 playNim board player

-- Life

cls :: IO ()
cls = putStr "\ESC[2J"

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [0..n]]

type Pos = (Int,Int)

writeat :: Pos -> String -> IO ()
writeat p xs =
  do goto p
     putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 50
height :: Int
height = 50

type BoardLife = [Pos]

glider :: BoardLife
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

showcells :: BoardLife -> IO ()
showcells board = sequence_ [writeat p "X" | p <- board]

isAlive :: BoardLife -> Pos -> Bool
isAlive board p = elem p board

isEmpty :: BoardLife -> Pos -> Bool
isEmpty board p = not (isAlive board p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1, y-1), (x, y-1), (x+1, y-1),
                          (x-1, y)  ,           (x+1, y)  ,
                          (x-1, y+1), (x, y+1), (x+1, y+1)]

wrap :: Pos -> Pos
wrap (x,y) = ( ((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

liveneighbs :: BoardLife -> Pos -> Int
liveneighbs board = length . filter (isAlive board) . neighbs

survivors :: BoardLife -> [Pos]
survivors board = [p | p <- board, elem (liveneighbs board p) [2,3]]

births :: BoardLife -> [Pos]
births board = [p | p <- rmdups (concat [neighbs s | s <- board]),
                   isEmpty board p,
                   liveneighbs board p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

nextgen :: BoardLife -> BoardLife
nextgen board = survivors board ++ births board

life :: BoardLife -> IO ()
life board = 
  do cls
     showcells board
     wait 160000
     life (nextgen board)

main :: IO ()
main = life glider


-- Exercises
-- 1)
putStr' :: String -> IO ()
putStr' xs =
  do sequence_ [putChar x | x <- xs]

putStrLn' :: String -> IO ()
putStrLn' xs =
  do putStr' xs
     putChar '\n'

-- 2) See implementation above
--
-- 3) 
putBoard'' :: Board -> IO ()
putBoard'' xs = sequence_ [putRow row x | (row, x) <- zip [1..] xs]

-- 4)
adder' :: Int -> Int -> IO ()
adder' runningTot rem = 
  if rem < 1 then
     do putStr "The total is "
        putStrLn (show runningTot)
  else
     do num <- getLine
        if isInt num then
           adder' (runningTot + (read num :: Int)) (rem-1)
        else
           do putStrLn "ERROR: Invalid number. Integers only. Try again."
              adder' runningTot rem


isInt :: String -> Bool
isInt [] = True
isInt (x:xs) = (isDigit x || x == '-') && all (isDigit) xs

adder :: IO ()
adder = 
  do putStr "How many numbers? "
     nums <- getLine
     if isInt nums then
        adder' 0 (read nums :: Int)
     else
        do putStrLn "ERROR: Invalid number. Integers only. Negatives terminate"
           adder

-- 5)
{-
getInt :: IO (Maybe Int)
getInt =
  do num <- getLine
     if isInt num then 
        return (Just (read num :: Int))
     else 
        do putStrLn "ERROR: Not an Int"
           return Nothing
-}
getInt :: IO Int
getInt =
  do num <- getLine
     if isInt num then 
        return (read num :: Int)
     else 
        return 0        

adder'' :: IO ()
adder'' =
  do putStr "How many numbers? "
     nums <- getLine
     if isInt nums then
        do toAdd <- (sequence [getInt | x <- [1..(read nums :: Int)]])
           putStr "The total is "
           putStrLn (show (sum toAdd))
     else
        do putStrLn "ERROR: Invalid number. Integers only. Negatives terminate"
           adder''

-- 6)
readLine'' :: String -> IO String
readLine'' xs = 
  do x <- getCh
     putChar x
     if x == '\n' then
        return xs
     else if x == '\DEL' then
        do putStr "\b \b"
           ys <- readLine'' ( reverse ( drop 1 (reverse xs) ))
           return ys
     else
        do ys <- readLine'' (xs++[x])
           return ys

readLine' :: IO String
readLine' = readLine'' ""
