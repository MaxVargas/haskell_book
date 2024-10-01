data Expr = Val Int | Add Expr Expr
data Op = PUSH Int | ADD
  deriving Show

eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y

type Stack = [Int]
type Code = [Op]

exec :: Code -> Stack -> Stack
exec []           s           = s
exec (PUSH n : c) s           = exec c (n : s)
exec (ADD : c)    (m : n : s) = exec c (n+m : s)

{-
comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = (comp x) ++ (comp y) ++ [ADD]
-}

comp' :: Expr -> Code -> Code
comp' (Val n)   c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

comp :: Expr -> Code
comp e = comp' e []

e = Add (Add (Val 2) (Val 3)) (Val 4)
