module Main where

import Data.Char (isAlpha)

react :: String -> IO ()
react [] = putStrLn "There was nothing on the input!!!"
react (a : _)
  | isAlpha a =
    putStrLn $
      "The first letter is "
        ++ if a `elem` "klmno"
          then "suspiciously close to the middle of the alphabet!"
          else "a completely uninteresting letter."
  | otherwise = putStrLn "The first character was not a letter!!!"

main :: IO ()
main = do
  firstLine <- getLine
  let lineLength = 78
  putStrLn $ replicate lineLength '*' -- output decorations don't make much sense and no one should use them...
  react firstLine
  putStrLn $ take lineLength $ cycle "---8<---" -- ...but at least the program doesn't look so boring.

{- An easy way to understand how IO works (for now):
 -
 - `getLine` has type `IO String`, which means that it is a description of some
 - kind of IO action that creates ("returns") a single String.
 -
 - The "binding" arrow syntax ( <- ) will create an extended description of a
 - larger IO action that will extract the "result" from the action on the
 - right, and make it available as a normal value to the following actions.
 - (You can imagine it as a way to "remove" the IO from the type.)
 -
 - In our case, on the line `firstLine <- getLine`, the types are as follows:
 -
 - getLine :: IO String
 - firstLine :: String
 -
 - (Compare that to `let lineLength = 78` -- let-bindings do _not_ "run" any
 - actions, and the types on both sides of the = are the same.)
 -
 - Other actions in a block that is started by `do` are connected in a way that
 - they execute in the correct order (first to last). In our case, that creates
 - one big description of an IO action, which is called `main`.
 -
 - Function `react` only describes one such action, thus `do` is not required
 - there. Also, it takes one parameter and returns a description of an IO
 - action that does not produce anything, so the type is correspondingly
 - `String -> IO ()`.
 -
 - A more detailed description of what is actually happening inside will be
 - given at the second lecture.
 -}
