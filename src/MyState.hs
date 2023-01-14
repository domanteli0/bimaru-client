{-# LANGUAGE InstanceSigs #-}
module MyState(MyState, put, get) where

newtype MyState s a = MyState {
  runMyState :: s -> (a, s)
}

instance Functor (MyState s) where
  fmap :: (a -> b) -> MyState s a -> MyState s b
  f `fmap` state = MyState $ \oldState ->
    let (a, s) = runMyState state oldState
    in (f a, s)

instance Applicative (MyState s) where
  pure :: a -> MyState s a
  pure input = MyState $ \s -> (input, s)

  (<*>) :: MyState s (a -> b) -> MyState s a -> MyState s b
  f <*> state = MyState $ \oldState ->
    let (f', newState) = runMyState f oldState
        (a, newState') = runMyState state newState
    in (f' a, newState')

instance Monad (MyState s) where
  (>>=) :: MyState s a -> (a -> MyState s b) -> MyState s b
  state >>= f = MyState $ \oldState ->
    let (a, newState) = runMyState state oldState
        (b, newState') = runMyState (f a) newState
    in (b, newState')

-- put s
put :: s -> MyState s ()
put s = MyState $ const ((), s)

-- get s
get :: MyState s s
get = MyState $ \s -> (s, s)
