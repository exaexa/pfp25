{-# LANGUAGE RankNTypes #-}

import Data.Function ((&))
import Data.Functor.Const
import Data.Functor.Identity
import Data.List (group)
import Data.Monoid

-- this type is a good illustration but a bit too restrictive for the advances
-- lenses, so we won't use it explicitly.
type Lens''' b s = forall f. Functor f => (s -> f s) -> (b -> f b)

data Two a =
  Two a a
  deriving (Show)

x wrap (Two a b) = fmap (\aa -> Two aa b) $ wrap a

y wrap (Two a b) = fmap (\bb -> Two a bb) $ wrap b

--lens :: (a -> b) -> (a -> b -> a) -> Lens a b
lens getter setter wrap a = fmap (setter a) $ wrap (getter a)

--set :: Lens a b -> b -> a -> a
set l v = runIdentity . l (\_ -> Identity v)

--over :: Lens a b -> (b->b) -> a -> a
over l f = runIdentity . l (Identity . f)

--view :: Lens a b -> a -> b
--view l = getConst . l (\b -> Const b)
view l = getConst . l Const

-- this is like view but slightly more generic
get l f = getConst . l (Const . f)

--two :: Applicative f => (a -> f a) -> (Two a -> f (Two a))
two wrap (Two a b) = Two <$> wrap a <*> wrap b

listOf l = getConst . l (\x -> Const [x])

-- more generic listOf (any monoid works)
-- 
-- nb.: listOf l == foldMapOf l (:[])
foldMapOf l f = getConst . l (Const . f)

-- target filter
filtered cond f s =
  if cond s
    then f s
    else pure s

-- note that this requires the explicit type (otherwise the compiler won't know
-- that it's supposed to read the same type as the one getting shown)
asString :: (Read a, Show a) => Lens''' a String
asString wrap a = fmap read $ wrap (show a)

-- traverse a number as a string
demo1 = 1234321 & set (asString . traverse . filtered (== '2')) '5'

-- we can add indexes to lenses by changing the representation a little
--
-- addIndexes :: Applicative f => ((Int, a) -> f (Int, b)) -> [a] -> f [b]
withIndexes wrap = traverse (fmap snd . wrap) . zip [0 :: Int ..]

-- a helper for making functions that work with indices
ix f (i, c) = (i, f i c)

demo2 = "hullow" & listOf withIndexes

-- this can (among other) change the type of the big object
demo2b = "oh hello" & over withIndexes (ix $ replicate . succ)

-- a lens for 2-tuples. In usual lens libraries, this is called _1 and _2.
first wrap (a, b) = fmap (flip (,) b) (wrap a)

second wrap (a, b) = fmap ((,) a) (wrap b)

-- a lens for the n-th item in a list
nth i wrap xs =
  let (as, bs) = (take i xs, drop i xs)
   in fmap ((as ++) . (: bs)) $ wrap (head bs)

-- how can we pull normal lenses through indexed values?
-- ix'd :: Lens b s -> Lens (i,b) (i,s)
--
-- exercise: can this get generalized to any isomorphism?
ix'd l wrap (i, x) = fmap ((,) i) $ l (fmap snd . wrap . ((,) i)) x

-- a small helper, like a `set` but sets the target depending on the index
ixset l a = over l (ix $ const . a)

-- Example: If `fst` is smaller than 10 in N-th tuple, then we set 4th
-- character in `snd` to the N-th character of the alphabet.
--
-- The trick: `ix'd` ensures that the lenses therein are able to ignore the
-- index decoration.
demo3 =
  [(5, "asdasd"), (123, "sdfsdf"), (2, "dfgdfg")]
    & ixset
        (withIndexes . ix'd (filtered ((< 10) . fst) . second . nth 3))
        (['A' ..] !!)
