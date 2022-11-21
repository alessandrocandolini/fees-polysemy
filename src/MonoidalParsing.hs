{-# LANGUAGE DerivingVia #-}
module MonoidalParsing where

data Balance = Balance Int Int deriving (Eq, Show)

instance Semigroup Balance where
  (<>) (Balance a b) (Balance c d) 
     | b <= c = Balance (a + c -b ) d 
     | otherwise = Balance a (d + b - c) 

instance Monoid Balance where
  mempty = Balance 0 0

data List a = Nil | Cons a (List a) deriving (Eq, Show) 

instance Applicative List where 
  pure a = Cons a Nil

  -- app :: f (a -> b) -> f a -> f b 
  (<*>) :: List (a -> b) -> List a -> List b 
  (<*>) = ... 
  List(1,2,3) <*> List(33,4,,5, 7) 

-- semigroup JDG
-- certain type of problems 
--

-- ()  = 0 
-- )( != 0 
(0, 1) <> (1, 0) = (0, 0) 
(1, 9) <> (0, 1) != (0, 0) 


parser :: Char -> Balance
parser '(' = Balance 0 1
parser ')' = Balance 1 0
parser _ = mempty


balance :: String -> Balance
balance = foldMap parser


balanced = (mempty ==) . balance 


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
