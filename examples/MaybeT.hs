newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT m) = MaybeT $ fmap (fmap f) m

instance Monad m => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  MaybeT a <*> MaybeT b = MaybeT $ a >>= maybe (return Nothing) ((<$> b) . fmap)

instance Monad m => Monad (MaybeT m) where
  return = pure
  MaybeT a >>= f = MaybeT $ a >>= maybe (return Nothing) (runMaybeT . f)

liftThroughMaybeT :: Monad m => m a -> MaybeT m a
liftThroughMaybeT a = MaybeT (return <$> a)

failMaybeT :: Monad m => MaybeT m a
failMaybeT = MaybeT (return Nothing)

io :: IO a -> MaybeT IO a
io = liftThroughMaybeT

main =
  runMaybeT $ do
    io $ putStrLn "give me a number!"
    a <- io $ (readLn :: IO Float)
    if a < 0
      then failMaybeT
      else io $ putStrLn "square root exists!"
    io . print $ sqrt a
