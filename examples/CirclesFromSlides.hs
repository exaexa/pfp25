import Graphics.Gloss

{-
 - This is the circle-drawing program that appears in slides.
 -}
main =
  animate FullScreen white $ \t ->
    let blink ph sp = (1 + sin (ph + t * sp)) / 2
     in Pictures
          $ flip map [1 .. 4]
          $ \x ->
              Rotate (90 * x + 20 * t)
                $ Translate (80 + 40 * cos t) 0
                $ Color (makeColor (blink 0 1) (blink 3 1) (blink 0 pi) 1)
                $ ThickCircle 20 3
