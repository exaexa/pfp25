import Prelude
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

{- The state of the world is represented simply with a single number -}
initialWorld = 5

{- This function draws the world (integer `n`) as a Gloss `Picture` type.
 - (see the documentation for the Picture type on Hoogle.) -}
drawWorld n =
  Color black $
  Pictures $
  flip map [1 .. n] $ \i ->
    Translate (100 * fromInteger i - 550) 0 $ Pictures [ThickCircle 50 10]

{- This function changes the world (integer `n`) based on an incoming event, in
 - our case arrow keys being pressed.a -}
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) n = max 0 $ n - 1
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) n = min 10 $ n + 1
handleEvent _ n = n -- we ignore all other events

{- This function is supposed to update the world regularly after some time
 - interval passes. The parameter would be the time difference to cover with
 - the update (we discard it with `_`), and the function would be able to
 - change the initial state (and we don't change anything by returning `id`).
 -
 - Unless you want actual animated things, you can leave this as is.
 -}
updateWorld _ = id

{- Function `play` from gloss connects the functions for managing and drawing
 - the world state and runs them on the initial state, with a selected
 - background color and framerate. All other things are handled by the Gloss
 - library. -}
main = play FullScreen white 25 initialWorld drawWorld handleEvent updateWorld
