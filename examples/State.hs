import Control.Monad

{-
 - An example implementation of State monad (as in slides)
 -
 - Note a difference from the standard library-- the State from package `mtl`
 - or `transformers` has the result flipped, the state changing function is `s
 - -> (a,s)` instead for `s -> (s,a)`. The reasons for this are subtle.
 -}
newtype State s a =
  StateChange (s -> (s, a))

-- Making the State from above a Functor, Applicative and Monad
instance Functor (State s) where
  fmap func (StateChange f) = StateChange (fmap func . f)

instance Applicative (State s) where
  pure a = StateChange (\s -> (s, a))
  StateChange f <*> StateChange v =
    StateChange
      (\s ->
         let (s1, a1) = f s
             (s2, a2) = v s1
          in (s2, a1 a2))

instance Monad (State s) where
  return = pure
  StateChange f1 >>= f2 =
    StateChange
      (\s ->
         let (s', a) = f1 s
             StateChange f3 = f2 a
          in f3 s')

-- action that sets the global state to a given value
put :: s -> State s ()
put ns = StateChange $ const (ns, ())

-- action that retrieves the global state
get :: State s s
get = StateChange (\s -> (s, s))

-- action that applies a function to change the global state
modify :: (s -> s) -> State s ()
modify f = StateChange (\s -> (f s, ()))

-- helpers for executing the stateful computations from an initial state
runState (StateChange f) = f
execState (StateChange f) = snd . f

-- demo "stateful factorial" from slides
fact n = execState (factS n) 1

factS n
  | n <= 1 = get
  | otherwise = do
    modify (* n)
    factS (n - 1)

-- demo "random number generator" from slides
getRand :: Int -> State Int Int
getRand max = do
  newSeed <- (12345 +) . (1103515245 *) <$> get
  put newSeed
  return $ (newSeed `div` 65536) `mod` max

get2Rand max = (,) <$> getRand max <*> getRand max

getNRand max 0 = pure []
getNRand max n = (:) <$> getRand max <*> getNRand max (n - 1)

-- run this for a demo:
getNRandDemo = execState (getNRand 1000 10) 12345 -- the last number is the initial random seed

-- more concise versions:
getNRand' max n = mapM (\_ -> getRand max) [1 .. n]

getNRand'' max n = replicateM n (getRand max)

getNRand''' = flip replicateM . getRand

-- Point-free version of get2Rand is below. Notably, functional programs are
-- always possible to write without any arguments, but in this case just don't.
get2Rand' = (<*>) . ((,)<$>) . getRand <*> getRand
