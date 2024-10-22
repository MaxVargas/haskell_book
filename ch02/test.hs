-- Doubles a value
double x = x + x

-- Quadruples it
quadruple x = double (double x)

-- Gets factorial
factorial n = product [1..n]

-- Average of a list
average ns = sum ns `div` length ns

-- N
n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]

-- Create a list from 0 to n 
zeroto :: Int -> [Int]
zeroto n = [0..n]

-- Apply a function to a value
apply :: (a -> b) -> a -> b
apply f x = f x

