import Data.Monoid
import Data.Foldable

{- 1) Product of monoids
instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
  -- mappend :: Semigroup (a,b) -> Semigroup (a,b) -> Semigroup (a,b)
  (x1, y1) <> (x2, y2) = (x1<>x2, y1<>y2)

instance (Monoid a, Monoid b) => Monoid (a,b) where
  -- mempty :: (a,b)
  mempty = (mempty, mempty)
-}

{- 2) Monoid on functions a->b, where b is a monoid
instance Semigroup b => Semigroup (a->b) where
  -- mappend :: Semigroup (a->b) -> Semigroup (a->b) -> Semigroup (a->b)
  f <> g = (\x -> (f x) <> (g x))

instance Monoid b => Monoid (a->b) where
  -- mempty :: Monoid (a->b)
  mempty = (\x -> mempty)
-}

{- 3) Maybe is a foldable and traversable
instance Foldable Maybe where
  -- fold :: Monoid a => Maybe a -> a
  fold Nothing  = mempty
  fold (Just x) = x

  -- foldMap :: Monoid b => (a->b) -> Maybe a -> b
  foldMap _ Nothing  = mempty
  foldMap f (Just x) = f x

  -- foldr :: (a->b->b) -> b -> Maybe a -> b
  foldr _ y Nothing  = y
  foldr o y (Just x) = o x y

  -- foldl :: (a->b->a) -> a -> Maybe b -> a
  foldl _ x Nothing  = x
  foldl o x (Just y) = o x y

instance Traversable Maybe where
  -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing  = Nothing
  traverse g (Just x) = pure Just <*> f x
-}

-- 4) Tree is foldable and traversable
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Functor Tree where
  -- fmap :: (a->b) -> Tree a -> Tree b
  fmap _ Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  -- fold :: Monoid a => Tree a ->  a
  fold Leaf         = mempty
  fold (Node l x r) = (fold l) <> x <> (fold r)

  -- foldMap :: Monoid b => (a->b) -> Tree a -> b
  foldMap _ Leaf         = mempty
  foldMap g (Node l x r) = (foldMap g l) <> (g x) <> (foldMap g r)

  -- foldr :: (a->b->b) -> b -> Tree a -> b
  foldr _  y Leaf         = y
  foldr op y (Node l x r) = foldr op (op x (foldr op y r)) l

  -- foldl :: (a->b->a) -> a -> Tree b -> a
  foldl _  x Leaf         = x
  foldl op x (Node l y r) = foldl op (op (foldl op x l) y) r

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf         = pure Leaf
  traverse g (Node l x r) = pure Node <*> traverse g l <*> g x <*> traverse g r

-- 5) define a generic version of filter :: (a->Bool) -> [a] -> [a]
--    that can be used with any foldable type

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = foldMap (\x -> if f x then [x] else mempty)

-- filterF p x = filter p (toList x)
