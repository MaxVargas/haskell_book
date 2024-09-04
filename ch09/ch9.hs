data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Mul x y = x * y
apply Sub x y = x - y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

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

isIn :: Eq a => a -> [a] -> Bool
isIn x [y] = x==y
isIn x (y:ys) = x==y || isIn x ys

rmElem :: Eq a => a -> [a] -> [a]
rmElem x [] = []
rmElem x (y:xs) = if x==y then xs else (y : rmElem x xs)

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) ys = if isIn x ys && isChoice xs (rmElem x ys) 
  then True else False

pairMap :: (a->c) -> (b->d) -> [(a,b)] -> [(c,d)]
pairMap f g zs = [(f x, g y) | (x,y) <- zs]

-- 3) Chainging split to return ([], xs) and (xs, [])
--    would result in non-termination
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = [([x],xs)] ++ (pairMap (x:) id (split xs))


exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                  l <- exprs ls, 
                  r <- exprs rs,
                  e <- combine l r]

ops = [Add, Sub, Mul, Div]
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | nsSub <- choices ns, e <- exprs nsSub,
                      eval e == [n]]

type Result = (Expr, Int)
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns,
                     lx       <- results ls,
                     ry       <- results rs,
                     res      <- combine' lx ry]

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /=1 && x <= y
valid' Div x y = y /=1 && x `mod` y == 0

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m==n]




main :: IO()
-- Counting number of solutions to example problem = 780
--main = print (length ( solutions' [1,3,7,10,25,50] 765))

-- Counting expressions and valid ones too
{-
main = print (length [e | ns <- choices [1,3,7,10,25,50],
                          e  <- exprs ns],
              length [e | ns <- choices [1,3,7,10,25,50],
                          e  <- exprs ns,
                          length (eval e) > 0])
-}

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n, n) | n > 0]
results' ns  = [res | (ls, rs) <- split ns,
                     lx       <- results' ls,
                     ry       <- results' rs,
                     res      <- combine'' lx ry]
combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid'' o x y]
valid'' :: Op -> Int -> Int -> Bool
valid'' Add _ _ = True
valid'' Sub _ _ = True
valid'' Mul _ _ = True
valid'' Div x y = y /= 0 && x `mod` y == 0


-- Counting the number of valid expressions as above,
-- allowing for arbitrary integers
main = print (length [e | ns' <- choices [1,3,7,10,25,50],
                         (e,m) <- results' ns'])

--main = print(length (solutions' [1,3,7,10,25,50] 765))


