module Common.State where

newtype State s a =
  State { runState :: s -> (s, a) }

instance Functor (State s) where

  fmap f (State run) = State $ \s ->
    let (s',   a) = run s
    in  (s', f a)

instance Applicative (State s) where

  pure a = State $ \s -> (s, a)

  sf <*> sx = State $ \s ->
    let ( s',   f) = runState sf s
        (ss',   x) = runState sx s'
    in  (ss', f x)

instance Monad (State s) where

  return = pure

  sx >>= sf = State $ \s ->
    let (s', x) = runState sx s
        State r = sf x
    in r s'

put :: a -> State a ()
put x = State $ \_ -> (x, ())

get :: State a a
get = State $ \s -> (s, s)

modify :: (a -> a) -> State a ()
modify f = do
  x <- get
  put $! f x

evalState :: State s a -> s -> a
evalState m = snd . runState m 

execState :: State s a -> s -> s
execState m = fst . runState m
