{-# LANGUAGE DerivingVia #-}
module Todo where
--import Prelude hiding (Semigroup, Monoid)


newtype AdditiveInteger = AdditiveInteger Integer 
  deriving (Eq,Show) 
  deriving Num via Integer

class MySemigroup a where
  phi :: a -> a -> a

class MySemigroup a => MyMonoid a where
  zero :: a


instance Semigroup AdditiveInteger where
   (<>) = (+)

instance Monoid AdditiveInteger where
   mempty = 0


instance MySemigroup [a] where
   phi = (++)
  
instance MyMonoid [a] where 
   zero = []

data B = B Int Int deriving (Eq,Show)

instance Semigroup B where
  (<>) (B a b) (B c d) = B (a+c) (b +d) 

instance Monoid B where
  mempty = B 0 0
