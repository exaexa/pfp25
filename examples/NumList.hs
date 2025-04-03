instance Num a => Num [a] where
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) = zipWith (*)
  negate = map negate
  abs = map abs
  signum = map signum
  fromInteger = repeat . fromInteger

fibs = 0 : 1 : fibs + tail fibs
