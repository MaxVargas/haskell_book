data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap g (Node l y r) = Node (fmap g l) (g y) (fmap g r)

{- 2)
 - The below satisfies the functor laws
 - by the associativity of function composition
 - (f . g) . h = f . (g . h)
instance Functor ((->) a) where
  -- fmap :: (b->c) -> (a->b) -> (a->c)
  fmap g h = g . h

- 3)
- The below satisfies the applicative laws
- pure id :: (a->b->b) ; g : (a->b)
- pure id x y = y      ; g
- - pure id <*> g = (\x -> (pure id) x (g x))
-                 = (\x -> g x)
-                 = g
- - pure g <*> pure x = (\y -> (pure g) y (pure x y))
-                     = (\y -> (pure g) y x)
-                     = (\y -> g x) --look at the def of pure
-                     = pure (g x)
- In below, x :: a->(b->c) ; y :: a->b
- - pure (\g -> g y) <*> x = (\z -> (pure (\g -> g y)) z (x z))
-                          = (\z -> (x z) y) -- currying to the next step
-                          = (\z -> x z y)
-                          = (\z -> x z (pure y z))
-                          = x <*> pure y
- - x <*> (y <*> z) = x <*> (\t -> y t (z t))
-                   = (\u -> x u ((\t -> y t (z t)) u) )
-                   = (\u -> x u (y u (z u))) 
-                   = (\u -> ( (x u) . (y u) ) (z u) )
-                   = (\u -> ((\t -> (x t) . (y t)) u) (z u))
-                   = (\u -> (pure (.) <*> x <*> y) u (z u) )
-                   = (pure (.) <*> x <*> y) <*> z
instance Applicative ((->) a) where
  -- pure :: b -> (a->b)
  pure x = (\y -> x)

  -- (<*>) :: (a->(b->c)) -> (a->b) -> (a->c)
  f <*> g = (\x -> f x (g x))
-}

-- 4)
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a->b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)

  -- (<*>) :: ZipList (a->b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

-- 5) See ch12.hs

{- 6)
 - Define an instance of the Monad class for (a->_)
- Let's check that the below definition satisfies the required properties
- - return x >>= f = (\z -> f (return x z) z)
-                  = (\z -> f (pure x z) z)
-                  = (\z -> f x z)
-                  = f x
- - mx >>= return = (\z -> return (mx z) z)
-                 = (\z -> pure (mx z) z)
-                 = (\z -> mx z)
-                 = mx
- - mx >>= (\x -> (f x >>= g)) = mx >>= (\x -> (\t -> g (f x t) t))
-                              = (\u -> (\x -> (\t -> g (f x t) t)) (mx u) u)
-                              = (\u -> (\t -> g (f (mx u) t) t) u)
-                              = (\u -> g (f (mx u) u) u)
-                              = (\u -> f (mx u) u) >>= g
-                              = (mx >>= f) >>= g

instance Monad ((->) a) where
  -- return :: b -> (a->b)
  return = pure

  -- (>>=)  :: (a->b) -> (b -> (a->c) ) -> (a->c)
  f (>>=) g = (\x -> g (f x) x)
-}

-- 7)
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  -- fmap :: (a->b) -> Expr a -> Expr b
  fmap g (Var x) = Var (g x)
  fmap g (Val x) = Val x
  fmap g (Add x y) = Add (fmap g x) (fmap g y)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure x = Var x

  -- (<*>) :: Expr (a->b) -> Expr a -> Expr b
  (Var f) <*> (Var x) = Var (f x)
  (Var f) <*> (Val n) = Val n
  (Val n) <*> (Var x) = Val n
  (Val n) <*> (Val m) = Val (n*m)
  (Var f) <*> (Add x y) = Add ((Var f)<*>x) ((Var f)<*>y)
  (Val n) <*> (Add x y) = Add ((Val n)<*>x) ((Val n)<*>y)
  (Add f g) <*> x = Add (f<*>x) (g<*>x)

-- The bind operator is just substitution
-- But here's an example
-- f       = (\x -> Add (Var (succ x)) (Val 5))
-- e       = Add (Var 'x')               (Add (Val 2) (Var 'y'))
-- e >>= f = Add (Add (Var 'y') (Val 5)) (Add (Val 2) (Add (Var 'z') (Val 5)))
instance Monad Expr where
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (Val n) >>= f = Val n
  (Var x) >>= f = f x
  (Add x y) >>= f = Add (x >>= f) (y >>= f)

-- 8)
type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a->b) -> ST a -> ST b
  fmap g st =
    do x <- st
       return (g x)

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x,s))

  -- (<*>) :: ST (a->b) -> ST a -> ST b
  stf <*> stx = 
    do f <- stf
       x <- stx 
       return (f x)

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s ->
     let (x,s') = app st s in app (f x) s')



