count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x: xs) = x: filter (/=x) (rmdups xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = (x:(y:ys))
                | otherwise = (y: insert x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

result :: Ord a => [a] -> [(Int, a)]
result xs = isort [(count x xs, x) | x <- rmdups xs]

winner :: Ord a => [a] -> a
winner = snd . last . result

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

altAlgWinner :: Ord a => [[a]] -> a
altAlgWinner bs = case rank (rmempty bs) of
                    [c]  -> c
                    (c:cs) -> altAlgWinner (elim c bs)

-- A list comprehension [f x | x <- xs, p x]
-- Can be constructed as:
-- ( map f . filter p )

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

or' :: (a -> Bool) -> [a] -> Bool
or' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x xs -> if p x then x:xs else []) []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p = foldl (\xs x -> if length xs > 0 then xs++[x]
                               else if p x then []
                               else [x]) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

int2dec :: Int -> [Int]
int2dec z = [read [x] :: Int | x <- show z]

curry' :: ( (a,b)->c ) -> a->b->c
curry' f = (\x -> (\y -> f (x,y) ))

uncurry' :: (a->b->c) -> (a,b) -> c
uncurry' f = (\(x,y) -> f x y)

unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (==0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (==[]) (take 4) (drop 4)

map'' :: (a -> b) -> [a]->[b]
map'' f = unfold (null) (f.head) (tail)

a2False :: a -> Bool
a2False x = False

iterate' :: (a->a) -> a->[a]
iterate' f = unfold (a2False) (id) (f)

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x:odds xs

odds :: [a] -> [a]
odds (_:xs) = evens xs
odds _ = []

isEven :: [a] -> Bool
isEven = (==0) . (`mod` 2) . length

altMap :: (a->b) -> (a->b) -> [a]->[b]
altMap f g = foldl (\xs x -> if isEven xs then xs ++ [f x] else xs ++ [g x]) []

luhn :: Int -> Bool
luhn = (==0) . (`mod` 10) . sum . (map (\x -> if x>9 then x-9 else x)) . altMap (id) (*2) . reverse . int2dec
