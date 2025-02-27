module Main where

import Brick
import qualified Brick.Types as T
import Brick.Util (bg, fg, on)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Control.Monad (void)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as Color

{- 
 - ScreenState is our very simple data type that stores the state of the whole
 - application. We use the "record syntax" here -- that is a bit more
 - complicated that the usual algebraic datatype definition, but otherwise
 - equivalent and more similar to C-style structures. The names of "fields" are
 - materialized as accessor functions, e.g., curPos :: ScreenState -> Int
 -}
data ScreenState =
  ScreenState
    { curPos :: Int
    , curHighlighted :: Bool
    }

{- This creates the initial state of the app -}
initState = ScreenState {curPos = 0, curHighlighted = False}

{- This function converts the state to a list of Brick widgets, which will get
 - rendered to the user. Here we only have a single layer of the widgers. -}
renderApp s = [center (title <=> mainLayer s)]

{- Operator <=> stacks the widgets vertically,
 - operator <+> puts them next to each other horizontally. -}
mainLayer s = foldr1 (<+>) $ map (letter s) [0 .. 10]

title = border $ str "Brick demo app!"

cursorHl = attrName "cursorHl"

letter s i
  | curPos s == i =
    (if curHighlighted s
       then withAttr cursorHl
       else id) $
    str "X"
  | otherwise = str "-"

{- 
 - Event-handling.
 -
 - This function changes the application state based on an incoming event.
 - Here, all events are Vty ("terminal") events of type EvKey ("events that
 - concern keys").
 -
 - Brick provides 2 extra functions
 -
 - halt -- this stops the app at the final state
 - modify fn    -- this changes the state using a given function
 -
 - Also, we use the nicer record-update syntax, which works roughly as follows:
 -
 - oldData {fieldToChange=newValue}
 -}
handleEvent (T.VtyEvent (V.EvKey k [])) =
  case k of
    V.KEsc -> halt
    V.KLeft -> modify $ \s -> s {curPos = max 0 (curPos s - 1)}
    V.KRight -> modify $ \s -> s {curPos = min 10 (curPos s + 1)}
    V.KChar ' ' -> modify $ \s -> s {curHighlighted = not (curHighlighted s)}
    _ -> return () --ignore all other keys
handleEvent _ = return () --and ignore all other events and do nothing

{- This glues all the defined function to a single Brick app. -}
app :: App ScreenState e ()
app =
  App
    { appDraw = renderApp
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap =
        const $ attrMap V.defAttr [(cursorHl, Color.black `on` Color.cyan)]
    }

{- Function `main` basically just runs the `app` on the initial state. The
 - result (which is the final state) is simply discarded using `void`, so that
 - `main` can "return" a proper empty type `()`. -}
main :: IO ()
main = void $ defaultMain app initState
