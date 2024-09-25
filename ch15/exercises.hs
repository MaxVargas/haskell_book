{- 1) identifying redexes

1 + (2*3)
	2*3     -- inner
	1+(2*3) -- outer
(1+2) * (2+3)
	(1+2)       -- inner
	(2+3)       -- inner
	(1+2)*(2+3) -- outer
fst (1+2, 2+3)
	1+2    -- inner
	2+3    -- inner
	fst(,) -- outer
(\x -> 1+x) (2*3)
	2*3    -- inner
	(\x..) -- outer
-
- 2) Outermost evaluation is preferred for the expression
- fst (1+2, 2+3)
- because it does not require the evaluation of 2+3.
- Specifically, outermost evaluation proceeds as the sequence
- fst (1+2, 2+3)
- 1+2
- 3
- while innermost evaluation proceeds as
- fst (1+2, 2+3)
- fst (3, 2+3)
- fst (3, 5)
- 3
-
-
- 3) Consider the definition
- mult = \x -> (\y -> x * y)
- The evaluation of mult 3 4 can be broken down as
- mult 3 4
- (\x -> (\y -> x * y)) 3 4
- (\y -> 3 * y) 4
- 3 * 4
- 12
-}

-- 4) Construct the fibonacci numbers

fibs :: [Integer]
fibs = (0: 1 : xs)
  where
    xs = zipWith (+) fibs (tail fibs)

-- 5) Defining repeat, take, replicate for Tree

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

repeat' :: a -> Tree a
repeat' x = Node (repeat' x) x (repeat' x)

take' :: Int -> Tree a -> Tree a
take' 0 _            = Leaf
take' _ Leaf         = Leaf
take' n (Node l x r) = Node (take' (n-1) l) x (take' (n-1) r)

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat'

-- 6) Newton's method for sqrt

--        x         eps       sq(x)
sqroot :: Double -> Double -> Double
sqroot x eps = head [z | (y,z) <- zip apx (tail apx), abs (y-z) < eps]
  where
    apx = appxs x

appxs :: Double -> [Double]
appxs x = iterate (\y -> (y + x/y)/2) 1.0
