{-# LANGUAGE DeriveFunctor #-}
module Todo where
import Data.Time

data Todo = Todo String (Maybe UTCTime) deriving (Eq,Show)

data Free f a
  = Pure a
  | Free (f (Free f a)) deriving (Functor)

instance Functor f => Monad (Free f) where
  return = Pure
  Pure x  >>= g  =  g x
  Free fx >>= g  =  Free ((>>= g) <$> fx)

data Program a =
  GetCurrentTime (UTCTime -> Program a) |
  Persist Todo (Program a) |
  Done a deriving (Functor)

data ProgramF a =
  GetCurrentTimeF (UTCTime -> Program a) |
  PersistF Todo (Program a) |
  DoneF a deriving (Functor)

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap Pure

instance Applicative Program where
  pure = Done
  (<*>) g (Done a) = fmap (\f -> f a) g
  (<*>) g (Persist t next) = Persist t (g <*> next)


type Program' = Free ProgramF
persist'' :: Todo -> Program' ()
persist'' t = liftF (PersistF t (DoneF ()))


program'' :: Todo -> Program' ()
program'' t@(Todo _ Nothing) = Persist t (pure ())
program'' t@(Todo _ (Just due))=
  GetCurrentTime >>= \now ->
  if now >= due then
     Persist t (pure ())
  else
     pure () -- ignore errors


program' :: Todo -> Program ()
program' t@(Todo _ Nothing) = Persist t (pure ())
program' t@(Todo _ (Just due))=
  GetCurrentTime (\ now ->
  if now >= due then
     Persist t (pure ())
  else
     pure () -- ignore errors
     )


persist :: Todo -> IO()
persist = undefined

program :: Todo -> IO ()
program t@(Todo _ Nothing) = persist t
program t@(Todo _ (Just due))=
  getCurrentTime >>= \ now ->
  if now >= due then
     persist t
  else
     pure () -- ignore errors
