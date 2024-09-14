data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap g (Node l y r) = Node (fmap g l) (g y) (fmap g r)

{-
 - The below satisfies the functor laws
 - by the associativity of function composition
 - (f . g) . h = f . (g . h)
instance Functor ((->) a) where
  -- fmap :: (b->c) -> (a->b) -> (a->c)
  fmap g h = g . h

- The below satisfies the applicative laws
- pure id :: (a->b->b) ; g : (a->b)
- pure id x y = y      ; g
- - pure id <*> g = (\x -> pure id x g x = g x).
- - - Hence, pure id <*> g = g
- - pure g <*> pure x = (\y -> (pure g) y (pure x y))
-                     = (\y -> (pure g) y x)
-                     = (\y -> g x) --look at the def of pure
-                     = pure (g x)
- In below, x :: a->(b->c) ; y :: a->b
- - pure (\g -> g y) <*> x = (\z -> (pure (\g -> g y)) z (x z))
-                          = (\z -> (x z) y) 
-          x <*> y = (\z -> x z (y z))
instance Applicative ((->) a) where
  -- pure :: b -> (a->b)
  pure x = (\y -> x)

  -- (<*>) :: (a->(b->c)) -> (a->b) -> (a->c)
  f <*> g = (\x -> f x (g x))
-}

