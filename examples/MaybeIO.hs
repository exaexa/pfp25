newtype MaybeIO a = MaybeIO
  { runMaybeIO :: IO (Maybe a)
  }

instance Functor MaybeIO where
  fmap f (MaybeIO m) = MaybeIO $ fmap (fmap f) m

instance Applicative MaybeIO where
  pure = MaybeIO . pure . Just
  MaybeIO a <*> MaybeIO b =
    MaybeIO $ a >>= maybe (return Nothing) ((<$> b) . fmap)

instance Monad MaybeIO where
  return = pure
  MaybeIO a >>= f = MaybeIO $ a >>= maybe (return Nothing) (runMaybeIO . f)

liftIO :: IO a -> MaybeIO a
liftIO io = MaybeIO (return <$> io)

failMIO :: MaybeIO a
failMIO = MaybeIO (return Nothing)

main =
  runMaybeIO $ do
    liftIO $ putStrLn "give me a number!"
    a <- liftIO $ (readLn :: IO Float)
    if a < 0
      then failMIO
      else liftIO $ putStrLn "square root exists!"
    liftIO . print $ sqrt a
