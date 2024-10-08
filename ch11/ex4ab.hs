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

lowerDiags :: Grid -> Grid
lowerDiags g = [ [g!!(i+j)!!i | i <- [0..size-j-1]] | j <- [1..size-1]]

upperDiags :: Grid -> Grid
upperDiags g = [ [g!!i!!(i+j) | i <- [0..size-j-1]] | j <- [1..size-1]]

longestWhereEq :: Eq a => a -> [a] -> Int
longestWhereEq p ps = snd (foldl (longAgg p) (0,0) ps)

longAgg :: Eq a => a -> (Int, Int) -> a -> (Int, Int)
longAgg p (current, longest) p'
  | p'==p = (current+1, max (current+1) longest)
  | otherwise = (0, longest)

wins :: Player -> Grid -> Int -> Bool
wins p g winLength = any (>= winLength) (map line (rows ++ cols ++ dias))
  where
    line = longestWhereEq p
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)] 
           ++ (upperDiags g) ++ (lowerDiags g)
           ++ (upperDiags (map reverse g)) ++ (lowerDiags (map reverse g))

diag :: [[a]] -> [a]
diag g = [r!!i | (r,i) <- zip g [0..]]

won :: Grid -> Int -> Bool
won g winLength = wins O g winLength || wins X g winLength

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

prompt :: Player -> String
prompt p = "Player " ++ show p ++ "'s turn. Enter a move: "

-- Computer player
depth :: Int
depth = 9

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Int -> Tree Grid
gametree g p winLength = Node g [gametree g' (next p) winLength | g' <- moves g p winLength]

moves :: Grid -> Player -> Int -> [Grid]
moves g p winLength
  | won g winLength = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0..(size^2-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

minimax :: Tree Grid -> Int -> Tree (Grid, Player)
minimax (Node g []) winLength
  | wins O g winLength = Node (g, O) []
  | wins X g winLength = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts) winLength
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = [minimax t winLength| t <- ts]
    ps  = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Player -> Int -> Grid
bestmove g p winLength = head moves 
  where
    moves = [g' | Node (g', p') _ <- ts, p' == best]
    tree =  prune depth (gametree g p winLength)
    Node (_,best) ts = minimax tree winLength

getPlayerChoice :: IO Player
getPlayerChoice =
  do putStrLn "Play X or O? O goes first: "
     pChoice <- getLine
     case pChoice of
          "O" -> return O
          "X" -> return X
          _   -> do putStrLn "ERROR: Invalid choice. Try again"
                    getPlayerChoice

playgame :: IO ()
playgame = do hSetBuffering stdout NoBuffering
              pChoice <- getPlayerChoice
              winLength <- getNat "Specify the length of the winning line: "
              play empty O pChoice winLength

play :: Grid -> Player -> Player -> Int -> IO ()
play g p pChoice winLength =
  do cls
     goto (1,1)
     putStrLn ("Player piece: " ++ show pChoice)
     putStrLn ""
     putGrid g
     play' g p pChoice winLength

play' :: Grid -> Player -> Player -> Int -> IO ()
play' g p pChoice winLength
  | wins O g winLength = putStrLn "Player O wins!\n"
  | wins X g winLength = putStrLn "Player X wins!\n"
  | full g             = putStrLn "Tie!\n"
  | p == pChoice = do i <- getNat (prompt p)
                      case move g i p of
                           [] -> do putStrLn "ERROR: Invalid move" 
                                    play' g p pChoice winLength
                           [g'] -> play g' (next p) pChoice winLength
  | p /= pChoice = do putStr "Player X is thinking..."
                      (play $! (bestmove g p winLength)) (next p) pChoice winLength

-- Exercises
-- 1)
treeSize :: Tree a -> Int
treeSize (Node _ []) = 1
treeSize (Node _ ts) = 1 + sum [treeSize t | t <- ts]

ex1Sol :: IO ()
ex1Sol = putStrLn( show (treeSize (gametree empty O 3))) 

-- 2)
-- See above in

-- Executable
main :: IO ()
main = playgame 
