import Control.Monad

{-
 - An example implementation of State monad (as in slides)
 -}

newtype State s a =
  StateChange (s -> (s, a))

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

put :: s -> State s ()
put ns = StateChange $ const (ns, ())

get :: State s s
get = StateChange (\s -> (s, s))

modify :: (s -> s) -> State s ()
modify f = StateChange (\s -> (f s, ()))

runState (StateChange f) = f
execState (StateChange f) = snd . f

fact n = execState (factS n) 1

factS n
  | n <= 1 = get
  | otherwise = do
    modify (* n)
    factS (n - 1)

getRand :: Int -> State Int Int
getRand max = do
  newSeed <- (12345 +) . (1103515245 *) <$> get
  put newSeed
  return $ (newSeed `div` 65536) `mod` max

get2Rand max = (,) <$> getRand max <*> getRand max

getNRand max 0 = pure []
getNRand max n = (:) <$> getRand max <*> getNRand max (n - 1)

-- run this for a demo:
getNRandDemo = execState (getNRand 1000 10) 12345 -- random seed

-- more concise versions:
getNRand' max n = mapM (\_ -> getRand max) [1 .. n]

getNRand'' max n = replicateM n (getRand max)

getNRand''' = flip replicateM . getRand

-- Point-free version of get2Rand is below. Notably, functional programs are
-- always possible to write without any arguments, but in this case just don't.
get2Rand' = (<*>) . ((,)<$>) . getRand <*> getRand
