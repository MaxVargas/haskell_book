grid :: Int -> Int -> [(Int, Int)]
grid n m = [(a,b) | a <- [0..n], b <- [0..m]]

square :: Int -> [(Int, Int)]
square n = [(a,b) | (a,b) <- grid n n, a/=b]

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]

factors :: Int -> [Int]
factors n = [a | a <- [1..n], n `mod` a == 0]

perfects :: Int -> [Int]
perfects n = [a | a <- [1..n], a*2 == sum (factors a)]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- [(x,y) | x <- [1,2], y <- [3,4]]
-- Is the same as
-- concat [[(x,y) | x <- [1,2]] | y <- [3,4]]
--

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x [(x', i) | (x', i) <- zip xs [0..]]

scalerProduct :: Num a => [a] -> [a] -> a
scalerProduct xs ys = sum [x*y | (x,y) <- zip xs ys]


