halve :: [a] -> ([a], [a])
halve as | (length as) `div` 2 == 0 = (take (length as `div` 2) as, drop (length as `div` 2) as)
         | otherwise = (take ((length as `mod` 2)+1) as, drop ((length as `div` 2)+1) as)

third1 :: [a] -> a
third1 as = head (tail (tail as))

third2 :: [a] -> a
third2 as = as!!2

third3 :: [a] -> a
third3 (_: (_: (x:_))) = x

safetail1 :: [a] -> [a]
safetail1 as = if length as == 0 then as else tail as

safetail2 :: [a] -> [a]
safetail2 as | length as == 0 = []
             | otherwise = tail as

safetail3 :: [a] -> [a]
safetail3 (_: as) = as
safetail3 [] = []

conditionalAnd :: Bool -> Bool -> Bool
conditionalAnd a b = if a then (if b then True else False) else False

mult :: Num a => a -> a -> a -> a
mult = (\x -> (\y -> (\z -> x*y*z)))

luhnDouble :: Int -> Int
luhnDouble a = if x > 9 then x-9 else x
  where
    x = a*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = x `mod` 10 == 0
  where
    x = luhnDouble a + b + luhnDouble c + d

