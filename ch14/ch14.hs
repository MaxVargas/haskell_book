import Data.Monoid
import Data.Foldable

{-
instance Monoid a => Monoid (Maybe a) where
  -- mempty :: Maybe a
  mempty = Nothing

  -- mappend :: Maybe a -> Maybe a -> Maybe a
  Nothing `mappend` my      = my
  mx      `mappend` Nothing = mx
  Just x  `mappend` Just y  = Just (x `mappend` y)
-}

newtype Sum' a = Sum' a
  deriving (Eq, Ord, Show, Read)

getSum' :: Sum' a -> a
getSum' (Sum' x) = x

-- The definition of a monoid has been split up using
-- the construction of a semigroup to define composition
instance Num a => Semigroup (Sum' a) where
  -- mappend :: Sum' a -> Sum' a -> Sum' a
  (Sum' x) <> (Sum' y) = Sum' (x+y)

instance Num a => Monoid (Sum' a) where
  -- mempty :: Sum' a
  mempty = Sum' 0

  
