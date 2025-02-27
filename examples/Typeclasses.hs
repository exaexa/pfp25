
{-
 - Finite field over integers modulo 7
 -}
newtype I7 =
  I7 Int
  deriving (Show)

instance Num I7 where
  (+) = i7lift2 (+)
  (-) = i7lift2 (-)
  (*) = i7lift2 (*)
  negate = i7lift negate
  abs = id
  signum (I7 0) = I7 0
  signum _ = I7 1
  fromInteger i = I7 $ fromInteger i `mod` 7

i7lift f (I7 a) = I7 (f a `mod` 7)

i7lift2 f (I7 a) (I7 b) = I7 (f a b `mod` 7)
 
int7demo = I7 6 + I7 2 -- this overflows 7 and becomes 1

int7demo2 :: I7
int7demo2 = 1 + 2 + 3 + 4 -- the numeric literals convert to I7 automatically via fromInteger

{-
 - "Treat lists of numbers as numbers"
 -}
instance Num a => Num [a] where
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) = zipWith (*)
  negate = map negate
  abs = map abs
  signum = map signum
  fromInteger = repeat . fromInteger

listDemo = 2 * [1, 2, 5, 6] + 3

fibs = 0 : 1 : fibs + tail fibs

{-
 - Add extra infinite values to any type.
 -
 - (On a side note, just `deriving Ord` would probably do the same here.)
 -}
data Infinite a
  = MinusInf
  | Finite a
  | PlusInf
  deriving (Eq, Show)

instance Ord a => Ord (Infinite a) where
  compare (Finite a) (Finite b) = compare a b
  compare MinusInf MinusInf = EQ
  compare MinusInf _ = LT
  compare _ MinusInf = GT
  compare PlusInf PlusInf = EQ
  compare PlusInf _ = GT
  compare _ PlusInf = LT

infiniteValues :: [Infinite Int]
infiniteValues = [MinusInf, PlusInf, Finite 123]

infiniteDemo = MinusInf < Finite 321
