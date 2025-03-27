import Control.Monad
import qualified Data.Vector.Mutable as V

bublsort mv =
  let size = V.length mv
   in forM_ [0 .. size - 1] $ \_ ->
        forM_ [0 .. size - 2] $ \i -> do
          [left, right] <- mapM (V.read mv) [i, i + 1]
          unless (left <= right) $ V.swap mv i (i + 1)

demoVector = do
  v <- V.new 5
  zipWithM (V.write v) [0 ..] [5, 3, 2, 5, 1]
  return v

printV v = forM_ [0 .. V.length v - 1] $ (>>= print) . V.read v

main = do
  v <- demoVector
  bublsort v
  printV v
