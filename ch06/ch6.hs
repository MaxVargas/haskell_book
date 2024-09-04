fac :: Integral a => a -> a
fac n | n == 0    = 1
      | n > 0     = n * fac (n-1)
      | otherwise = 0

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

euclid :: Integral a => a -> a -> a
euclid 1 _ = 1 -- Added for speeding some special cases
euclid _ 1 = 1 -- There are more special cases obv...
euclid n m | n == m = n
           | n <  m = euclid n (m-n)
           | otherwise = euclid (n-m) m

and' :: [Bool] -> Bool
and' [] = True
and' (x: xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs: xss) = xs ++ concat' xss

(!!!) :: [a] -> Int -> a
(x:_)  !!! 0 = x
(_:xs) !!! n = xs !!! (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (x':xs) | x == x' = True
                | otherwise = elem' x xs 

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y    = (x: merge xs (y:ys) )
                    | otherwise = (y: merge (x:xs) ys )

halve :: [a] -> ([a], [a])
halve as | (length as) `mod` 2 == 0 = (take (length as `div` 2) as, drop (length as `div` 2) as)
         | otherwise = (take ((length as `div` 2)+1) as, drop ((length as `div` 2)+1) as)

mSort :: Ord a => [a] -> [a]
mSort [x] = [x]
mSort xs = merge (mSort xs1) (mSort xs2)
    where
        (xs1, xs2) = halve xs

