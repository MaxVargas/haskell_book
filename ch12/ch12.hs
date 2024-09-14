{-
pure :: a -> f a
<*> :: f (a->b) -> f a -> f b
-}

-- Type Reasoning
-- 1) pure id <*> x = x :: a
-- 
--    pure id :: a -> a
--    <*> :: Id (a -> b) -> Id a -> Id b
--        :: (a->b) -> a->b
--
--
-- 2) pure (g x) = pure g <*> pure x :: f b
--
--    g :: a->b, then g a :: b and pure (g a) :: f b
--    pure g :: f a -> f b and pure x :: f a
--       so pure g <*> pure x :: f b
--
-- 3) x <*> pure y = pure (\g -> g y) <*> x :: f b
-- 
--    x :: f (a->b), pure y :: f a, so LHS :: f b
--    (\g -> g y) :: (a->b) -> b, pure (\g -> g y) :: f((a->b)->b)
--       so.. pure (\g -> g y) <*> x :: f b
--
-- 4) x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z :: f c
--    y :: f (a->b), z :: f a, x :: f(b->c)
--       LHS :: f c
--    <*> :: f((b->c)->(a->b)->(a->c)) -> f(b->c) -> f((a->b)->(a->c))
--       (pure (.) <*> x) :: f((a->b)->(a->c))
--       (pure (.) <*> x <*> y) :: f(a->c)
--       (pure (.) <*> x <*> y) <*> z :: f c

prods :: [Int] -> [Int] -> [Int]
prods xs ys = pure (*) <*> xs  <*> ys

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a->b) -> ST a -> ST b
  -- Notation: let Y = Expr in f Y returns f Y, on the assignment of Y
  fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a->b) -> ST a -> ST b
  -- Note: There's an important ordering on applying stf first
  --       Due to the left associativity of functions
  stf <*> stx = S (\s -> 
    let (f',s')    = app stf s
        (x'', s'') = app stx s' in (f' x'', s'')
    )

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  stx >>= f = S (\s -> let (x',s') = app stx s in app (f x') s')

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- recursion label
rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n   = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'')= rlabel r n'

--applicative label
fresh :: ST Int
fresh = S (\n -> (n,n+1))

-- Recall that constructors inherit types
-- Leaf :: a -> Leaf a
-- Tree :: Tree a -> Tree a -> Tree a
alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- pure Leaf :: ST Leaf
-- app (pure Leaf n) s = (Leaf n, s)
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _)   = do n <- fresh      -- fresh >>= (\n ->
                       return (Leaf n) -- S (\s -> (Leaf , s)))
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')
