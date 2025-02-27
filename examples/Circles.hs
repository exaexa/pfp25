import Graphics.Gloss

{-
 - A simple recursive circle-drawing program.
 -
 - How to install Gloss?
 -
 -   cabal install gloss
 -
 - How to run the program?
 - (while importing the installed library manually, without creating a "project")
 -
 -   ghc -package gloss Circles.hs -o circles
 -   ./circles
 -}
main =
  animate FullScreen white $ \t ->
    rotate (5 * t) 6
      $ Translate 400 0
      $ rotate (20 * t) 5
      $ Translate 120 0
      $ rotate (-70 * t) 4
      $ Translate 40 0
      $ rotate (180 * t) 3
      $ Translate 15 0
      $ ThickCircle 4 4
  where
    rotate time n p =
      Pictures $ map (\a -> Rotate (time + a * 360 / n) p) [1 .. n]
