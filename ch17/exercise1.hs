data Expr =
  Val Int
  | Add Expr Expr
  | Throw
  | Catch Expr Expr
  deriving Show

data Code = 
  HALT 
  | PUSH Int Code 
  | ADD Code 
  | ERROR Code
  | EXCEPT Code
  deriving Show

type Stack = [Maybe Int]

{-
eval :: Expr -> Maybe Int
eval (Val n)   = Just n
eval (Add x y) = 
  case eval x of
    Just n -> case eval y of
      Just m  -> Just (n+m)
      Nothing -> Nothing
    Nothing -> Nothing
eval Throw    = Nothing
eval (Catch x h) = 
  case eval x of
    Just n  -> Just n
    Nothing -> eval h
-}

ex1 = Catch (Add (Val 1) (Val 2)) Throw
ex2 = Catch Throw (Add (Val 1) (Val 2))
ex3 = Add (Catch (Add (Val 4) Throw) (Add (Val 2) (Val 8))) (Val 5)
ex4 = Add (Catch (Add (Val 4) Throw) (Add Throw (Val 8))) (Val 5)

eval :: Expr -> Stack
eval e = exec (comp e) []

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n)     c = PUSH n c
comp' (Add x y)   c = comp' x (comp' y (ADD c))
comp' Throw       c = ERROR c
comp' (Catch x h) c = comp' x (comp' h (EXCEPT c))

exec :: Code -> Stack -> Stack
exec HALT s                        = s
exec (PUSH n c) s                  = exec c (Just n : s)
exec (ADD c)    (Just m : Just n : s) = exec c (Just (n+m) : s)
exec (ADD c)    (_      : _      : s) = exec c (Nothing : s)
exec (ERROR c)  s                     = exec c (Nothing : s)
exec (EXCEPT c) (_      : Just n : s) = exec c (Just n : s)
exec (EXCEPT c) (val    : Nothing: s) = exec c (val    : s)
