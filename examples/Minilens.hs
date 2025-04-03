{-# LANGUAGE RankNTypes #-}

import Data.Functor.Const
import Data.Functor.Identity
import Data.List (group)
import Data.Monoid

-- tohle je trochu zbytecne restriktivni, takze to nebudeme pouzivat
type Lens b s
   = forall f. Functor f =>
                 (s -> f s) -> (b -> f b)

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

-- trochu vic genericky view
get l f = getConst . l (Const . f)

--two :: Applicative f => (a -> f a) -> (Two a -> f (Two a))
two wrap (Two a b) = Two <$> wrap a <*> wrap b

listOf l = getConst . l (\x -> Const [x])

foldMapOf l f = getConst . l (Const . f) -- generictejsi

-- listOf l == foldMapOf l (:[])
filtered cond f s =
  if cond s
    then f s
    else pure s

asString :: (Read a, Show a) => Lens a String
asString wrap a = fmap read $ wrap (show a)

-- projdeme cislo jako string
demo1 = set (asString . traverse . filtered (== '2')) '5' 1234321

-- "pridani indexu" je jen jina zmena reprezentace
--addIndexes :: Applicative f => ((Int, a) -> f (Int, b)) -> [a] -> f [b]
withIndexes wrap = traverse (fmap snd . wrap) . zip [0 :: Int ..]

ix f (i, c) = (i, f i c) --helper na vyrabeni funkci co pracuji s indexy

demo2 = listOf withIndexes "ahoj"

-- tohle (mimo jine) zmeni typ uvnitr velkeho objektu
demo2b = over (withIndexes) (ix $ replicate . succ) "ahoj"

-- lensy pro dvojice, v Data.Lens se tohle jmenuje _1 a _2
first wrap (a, b) = fmap (flip (,) b) (wrap a)

second wrap (a, b) = fmap ((,) a) (wrap b)

-- lens pro konkretni prvek v seznamu
nth i wrap xs =
  let (as, bs) = (take i xs, drop i xs)
   in fmap ((as ++) . (++ bs) . pure) $ wrap (head bs)

-- jak skrz indexovane hodnoty protahnout normalni lensy?
--ixd :: Lens b s -> Lens (i,b) (i,s)
ixd l wrap (i, x) = fmap ((,) i) $ l (fmap snd . wrap . ((,) i)) x
-- cviceni: neslo by to genericky s libovolnym isomorfismem? :]

-- pomucka (`set` ktery prvek nastavuje v zavislosti na indexu)
ixset l a = over l (ix $ const . a)

-- Priklad: Pokud je `fst` v N-te dvojici mensi nez 10, tak na 4ty znak v `snd`
-- dej N-te pismeno abecedy.
-- Trik: `ixd` zajisti, ze lensy v zavorce ignoruji indexovou dekoraci.
demo3 =
  ixset
    (withIndexes . ixd (filtered ((< 10) . fst) . second . nth 3))
    (['A' ..] !!)
    [(5, "asdasd"), (123, "sdfsdf"), (2, "dfgdfg")]
