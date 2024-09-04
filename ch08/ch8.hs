data Nat = Zero | Succ Nat
           deriving(Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ(int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ( add m n )

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add n (mult m n)

data Tree a = Leaf a | Node (Tree a) a (Tree a)
              deriving(Show)

test_T = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a =>  a -> Tree a -> Bool
occurs x (Leaf y) = x==y
occurs x (Node l y r) | x==y = True
                      | x <y = occurs x l
                      | otherwise = occurs x r

-- This is more efficient because
-- (1) we only do one comparison of x and y
occurs' :: Ord a =>  a -> Tree a -> Bool
occurs' x (Leaf y) = x==y
occurs' x (Node l y r) | comp==EQ = True
                       | comp==LT = occurs' x l
                       | otherwise = occurs' x r
  where
    comp = compare x y

data Treee a = Leaff a | Nodee (Treee a) (Treee a)
               deriving(Show)

test_TT = Nodee (Nodee (Leaff 1) (Leaff 4)) (Nodee (Leaff 6) (Leaff 9))
test_TT2 = Nodee (Nodee (Leaff 1) (Nodee (Nodee (Leaff 2) (Leaff 6)) (Leaff 9))) (Nodee (Leaff 6) (Leaff 9))


count_leaves :: Treee a -> Int
count_leaves (Leaff _) = 1
count_leaves (Nodee l r) = count_leaves l +  count_leaves r

balanced :: Treee a -> Bool
balanced (Leaff _) = True
balanced (Nodee x y) = balanced x && balanced y && differ_by_one
  where
    differ_by_one = p == q-1 || p == q || p == q+1
    p = count_leaves x
    q = count_leaves y

split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x],[])
split xs = (take n xs, drop n xs)
  where
    n = (length xs) `div` 2

balance :: [a] -> Treee a
balance [x] = Leaff x
balance xs = Nodee (balance firsts) (balance seconds)
  where
    (firsts, seconds) = split xs

-- Extending the definitions here to work with multiplication
-- The original problem dealt with addition only
data Expr = Val Int | Add Expr Expr | Mul Expr Expr

folde :: (Int -> a) -> (a->a->a) -> (a->a->a) -> Expr -> a
folde f _ _ (Val n)   = f n
folde f g h (Add u v) = g (folde f g h u) (folde f g h v)
folde f g h (Mul u v) = h (folde f g h u) (folde f g h v)

eval :: Expr -> Int
eval = folde id (+) (*)

size :: Expr -> Int
size = folde (\x -> 1) (+) (+)

data Maybee a = Nothingg | Justt a

instance Eq a => Eq (Maybee a) where
  Nothingg == Nothingg = True
  Justt x  == Justt y  = x==y
  _        == _        = False

--instance Eq a => Eq [a] where
--  [] == [] = True
--  [] == _  = False
--  _  == [] = False
--  (x:xs) == (y:ys) = x==y && xs==ys 

-- Tautology Checker
type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k==k']

data Prop = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Disj Prop Prop
  | Imply Prop Prop
  | Equiv Prop Prop

type Subst = Assoc Char Bool

evall :: Subst -> Prop -> Bool
evall _ (Const b) = b
evall s (Var p) = find p s
evall s (Not p) = not (evall s p)
evall s (And p q) = (evall s p) && (evall s q)
evall s (Disj p q) = (evall s p) || (evall s q)
evall s (Imply p q) = evall s p <= evall s q
evall s (Equiv p q) = evall s p == evall s q

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = rmdups (vars p ++ vars q)
vars (Imply p q) = rmdups (vars p ++ vars q)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x: xs) = x: filter (/=x) (rmdups xs)

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where
    bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip v) (bools n)
  where
    v = vars p
    n = length v

isTaut :: Prop -> Bool
isTaut p = and [evall s p | s <- substs p]

test_Taut = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- Abstract Machine
data Op = EVAL (Expr, Action) | ACT Action Int
data Action = ADD | MUL

type Cont = [Op]

evalu :: Expr -> Cont -> Int
evalu (Val n)   c = execu c n
evalu (Add x y) c = evalu x (EVAL (y, ADD) : c)
evalu (Mul x y) c = evalu x (EVAL (y, MUL) : c)

execu :: Cont -> Int -> Int
execu []           n = n
execu (EVAL (y, a)  : c) n = evalu y (ACT a n : c)
execu (ACT ADD n : c) m = execu c (n+m)
execu (ACT MUL n : c) m = execu c (n*m)


value :: Expr -> Int
value e = evalu e []

test_e = Add (Mul (Val 4) (Add (Val 3) (Val 2))) (Add (Val 2) (Mul (Val 1) (Val 4)))

