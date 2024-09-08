import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

data Player = O | B | X deriving (Eq, Ord, Show)
type Grid = [[Player]]

next :: Player -> Player
next O = X
next X = O
next B = B

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full g = all (all (/=B)) g

turn :: Grid -> Player
turn g = if xs >= os then O else X
  where 
    xs = (length . filter (==X) . concat) g
    os = (length . filter (==O) . concat) g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (==p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

diag :: [[a]] -> [a]
diag g = [r!!i | (r,i) <- zip g [0..]]

won :: Grid -> Bool
won g = wins O g || wins X g

-- Display utilities
cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x,y) = putStr("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y:x:interleave x ys

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer p | p==B = ["   ", "   ", "   "]
             | otherwise = ["   ", " " ++ show p ++ " ", "   "]

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where
    bar = [replicate (3*size+2) '-']

-- Gameplay
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && (concat g)!!i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
  if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs, B:ys) = splitAt i (concat g)

chop :: Int -> [Player] -> Grid
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt =
  do putStr prompt
     xs <- getLine
     if xs /= [] && all isDigit xs then
        return (read xs :: Int)
     else
        do putStrLn "ERROR: Invalid number"
           getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p =
  do cls
     goto (1,1)
     putGrid g
     run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g   = putStrLn "Player O Wins!\n"
         | wins X g   = putStrLn "Player X Wins!\n"
         | full g     = putStrLn "Tie!\n"
         | otherwise = 
             do i <- getNat (prompt p)
                case move g i p of
                   [] -> do putStrLn "ERROR: Invalid move"
                            run' g p
                   [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ "'s turn. Enter a move: "

-- Computer player
depth :: Int
depth = 9

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0..(size^2-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g  = Node (g, O) []
  | wins X g  = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps  = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head moves 
  where
    moves = [g' | Node (g', p') _ <- ts, p' == best]
    tree =  prune depth (gametree g p)
    Node (_,best) ts = minimax tree

playgame :: IO ()
playgame = do hSetBuffering stdout NoBuffering
              play empty O

play :: Grid -> Player -> IO ()
play g p =
  do cls
     goto (1,1)
     putGrid g
     play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g   = putStrLn "Tie!\n"
  | p == O   = do i <- getNat (prompt p)
                  case move g i p of
                     [] -> do putStrLn "ERROR: Invalid move" 
                              play' g p
                     [g'] -> play g' (next p)
  | p == X   = do putStr "Player X is thinking..."
                  (play $! (bestmove g p)) (next p)

-- Exercises
-- 1)
treeSize :: Tree a -> Int
treeSize (Node _ []) = 1
treeSize (Node _ ts) = 1 + sum [treeSize t | t <- ts]

ex1Sol :: IO ()
ex1Sol = putStrLn( show (treeSize (gametree empty O))) 

-- Executable
main :: IO ()
main = playgame 
