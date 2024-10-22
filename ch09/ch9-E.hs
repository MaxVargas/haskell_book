data Op = Add | Sub | Mul | Div | Exp
data Expr = Val Int | App Op Expr Expr
type Result = (Expr, Int)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

instance Eq Op where
  Add == Add = True
  Sub == Sub = True
  Mul == Mul = True
  Div == Div = True
  Exp == Exp = True
  _   == _   = False

instance Ord Op where
  Add < _ = True
  Sub < Mul = True
  Sub < Div = True
  Sub < Exp = True
  Mul < Div = True
  Mul < Exp = True
  Div < Exp = True
  _   < _ = False

  b <= c = (b<c) || (b==c)
  b >  c = c < b
  b >= c = c <= b

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e       = "(" ++ show e ++ ")"

instance Eq Expr where
  Val n     == Val m        = n == m
  App o l r == App o' l' r' = o == o' && l == l' && r == r'
  _         == _            = False

p  (App o l r) = ( (length . values) (App o l r), o, l, r)
instance Ord Expr where
  Val n     < Val m        = n < m
  App o l r < App o' l' r' = (w,x,y,z) < (w',x',y',z')
    where
      (w,x,y,z) = p (App o l r)
      (w',x',y',z') = p (App o' l' r')
  _         < _            = False

  b <= c = (b<c) || (b==c)
  b >  c = c < b
  b >= c = c <= b

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /=1 && x <= y
valid Div x y = y /= 0 && y /=1 && x `mod` y == 0
valid Exp x y = y >= 0 && x > 1 
-- Use below for technical correctness
-- But be wary of incredibly slow runtime for Integer arithmetic
--valid Exp x y = y >= 0 && x > 1 && (toInteger x)^(toInteger y)<(toInteger 2)^(toInteger 63)

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Mul x y = x * y
apply Sub x y = x - y
apply Div x y = x `div` y
apply Exp x y = x ^ y

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

pairMap :: (a->c) -> (b->d) -> [(a,b)] -> [(c,d)]
pairMap f g zs = [(f x, g y) | (x,y) <- zs]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = [([x],xs)] ++ (pairMap (x:) id (split xs))

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns,
                     lx       <- results ls,
                     ry       <- results rs,
                     res      <- combine lx ry]

ops = [Add, Sub, Mul, Div, Exp]
combine :: Result -> Result -> [Result]
combine (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

-- Combinatorics
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = sxs ++ map (x:) sxs
  where
    sxs = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat(map (interleave x) (perms xs))

choices :: [a] -> [[a]]
-- concat . map perms . subs
choices xs = [us | ts <- subs xs, us <- perms ts]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x<=y then (x:y:ys) else (y:insert x ys)

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

-- Solutions
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, (e,m) <- results ns', m==n]

-- main :: IO()
-- main = print (length (solutions [1,3,7,10,25,50] 765))

-- Closest Solutions
lowers :: Int -> [Int]
lowers x = [x - y | y <- [1..x]]

uppers :: Int -> [Int]
uppers x = [x + y | y <- [0..]]

altMerge :: [a] -> [a] -> [a]
altMerge xs [] = xs
altMerge [] ys = ys
altMerge (x:xs) (y:ys) = x : y : altMerge xs ys

listNearbyNums :: Int -> [Int]
listNearbyNums x = altMerge (uppers x) (lowers x) 

nonzeroHead :: (a->Int) -> [a] -> a
nonzeroHead f xss = head [xs | xs <- xss, f xs > 0]

nearestSolutions :: [Int] -> Int -> (Int, [Expr])
nearestSolutions ns n = nonzeroHead (length . snd) [(m, solutions ns m) | m <- listNearbyNums n]

-- Using 11111 gives no solutions.
-- The algorithm continues, finding one solution to 11112
main :: IO()
--main = print (u, length v, map (length . values) (take 20 (iSort v)))
main = print (u, length v, take 20 (iSort v))
  where
    (u, v) = nearestSolutions [1,3,7,10,25,50] 10

