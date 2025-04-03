-- Stateful computation over IO monad that can be short-circuited.
-- This example uses manual lifting to show how annoying it is.
--
-- See StateIOBetter.hs for much less ugly auto-lifting example.
module Main where

import Control.Monad (when) --useful `if` without `else`
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

main =
  flip runStateT 0 . runMaybeT $ do
    lift $ modify (+ 1)
    lift . lift $ putStrLn "modified first time"
    lift $ modify (+ 1)
        -- we can mix IO and State in a single line
    x <- (+) <$> lift get <*> lift (lift readLn)
        -- If the user enters a number that is too small, this produces a
        -- Nothing, which kills the rest of the computation.
    when (x < 5) $ fail "nothing"
    lift . lift $ putStrLn $ "state now: " ++ show x
    lift $ modify (* 12345)
    lift get >>= lift . lift . print
