-- like StateIO.hs but we avoid writing the lifts manually everywhere.
--
-- Uses `monads-tf` package (which is a `mtl`-compatibility layer above
-- transformers package). You can also use `mtl` moreless directly here.
--
-- To avoid using even liftIO, you need to replace the Prelude with one from
-- package `classy-prelude`.
module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify, put)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State (runStateT)

main =
  flip runStateT 0 . runMaybeT $ do
    modify (+ 1)
    liftIO $ putStrLn "modified first time"
    modify (+ 1)
    x <- (+) <$> get <*> liftIO readLn
    when (x < 5) $ fail "nothing"
    liftIO . putStrLn $ "state now: " ++ show x
    modify (* 12345)
    get >>= liftIO . print
