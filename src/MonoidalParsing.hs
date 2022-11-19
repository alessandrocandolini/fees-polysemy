module MonoidalParsing where

data Balance = Balance Int Int deriving (Eq, Show)

instance Semigroup Balance where
  (<>) (Balance a b) (Balance c d) 
     | b <= c = Balance (a + c -b ) d 
     | otherwise = Balance a (d + b - c) 

instance Monoid Balance where
  mempty = Balance 0 0


parser :: Char -> Balance
parser '(' = Balance 0 1
parser ')' = Balance 1 0
parser _ = mempty


balance :: String -> Balance
balance = foldMap parser


balanced = (mempty ==) . balance 
