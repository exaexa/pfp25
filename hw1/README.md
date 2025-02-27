# Homework 1 -- Paintbrush

Make a small bitmap graphics editor with some primitive functionality. We won't be coding the whole Photoshop -- the editor will be reasonably constrained to a few basic functions:

- The bitmap has a small pre-set size -- for the homework purposes, you can choose any size, such as 32×32. (You can imagine that you're writing an icon editor for Windows 3.11.)
- The bitmap only contains three different colors:
  - an "empty" color (which means "transparent" or "white" as with paper)
  - a "light" color
  - a "dark" color (pick whichever hue of light and dark you like; for example light and dark blue)
- There is no way to save or restore the bitmap, and upon restarting the editor, the whole bitmap is erased. This reflects the transient and sublime nature of the art of icon-making.

## Task description

Code such an editor in Haskell, using any of the libraries for simplified creation of user interfaces. Either `gloss` or `brick` is recommended for that purpose (see below). If you feel brave, you can use any other library that can be found on hoogle.

The user should be able to control the whole program using a keyboard. In particular, arrows should move a "cursor" over the bitmap, and a several other keys would modify the bitmap:
- pressing the spacebar sets the "color" of the pixel under the cursor to transparent
- `x` sets the color of the pixel to the "light" color
- `z` sets the color of the pixel to the "dark" color

### Bonus tasks

- level 1: implement a few extra operations:
  - pressing `d` means "darker" -- makes the transparent pixels light and light pixels dark
  - pressing `s` means "shinier" -- makes the dark pixels light and light pixels transparent
  - pressing `i` means "invert" -- flips the dark and light pixel values, but leaves transparency alone
- level 2: pressing `r` (as a Rectangle) will start a rectangle-selecting mode, where the user specifies a rectangle; pressing a color button then fills the whole rectangle
- level 3: the rectangle-filling mode can be used also with the `d`, `s` and `i` operations to apply them everywhere over the rectangle

Optimally, organize the implementation of the bonus tasks in a way that minimizes the amount of code; ideally without writing a separate `if` branch for each combination of the shapes and operations.

When drawing the more complex shapes, the user should be able to see what is going to be modified by the operation (e.g., while selecting a rectangle, the affected pixels may be highlighted by a slight color change).

### Additional tasks if you are bored and want to explore

- level 4: add a few extra "shapes" that work just as with the rectangle, such as `l` for making lines and `c` or `e` for making circles and/or ellipses
- level 5: somehow smuggle Bresenham's line-drawing algorithm and a triangle rasterizer into your solution. (You might need to get creative with shape seleciton methods here.)

## How to do it?

Preferably, use one of the following UI libraries:

- [Gloss](https://hackage.haskell.org/package/gloss-1.13.1.2/docs/Graphics-Gloss.html) -- with Gloss, you will get a simple graphical window where you can draw vector graphics. The library additionally supports direct way to make applications (pretty good for many small games) via the function [`play`](https://hackage.haskell.org/package/gloss-1.13.1.2/docs/Graphics-Gloss.html#v:play).
- [Brick](https://github.com/jtdaugherty/brick/) -- similar to Gloss, but instead of a graphical window you get a terminal window for nicely drawing ASCII graphics.

Do not over-do the graphical representations of the bitmap, cursor, selected region, etc. In case of Gloss, colored squares and a triangle arrow instead of a cursor are perfectly OK. In case of Brock, use simple ASCII representation of the bitmap -- `.` is empty, `#` is the light color, etc.

For both cases, you are advised to start from small applications that "already do something" and just plug in your logic. You are provided with such starters:

- [gloss.hs](./gloss.hs) -- this needs the `gloss` library; you can install it using `cabal install gloss`. You can compile the program in a stand-alone manner using  `ghc -package gloss gloss.hs -o gloss`
- [brick.hs](./brick.hs) -- this needs libraries `brick` a `vty` (for terminal colors). After installing them with `cabal`, you can compile the app using `ghc -package brick -package vty brick.hs -o brick`.

### Data structures

You can perfectly represent the whole bitmap as a list of lists (we do not evaluate efficiency). It is advised to make your own type for the pixels (instead of relying on integers etc.).

If you want to try a more compact approach, you can try the vectors from library [`Data.Vector`](https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html). Vectors are technically equivalent to a normal C-style array.

## Submission

You should submit your solution in a `cabal` package archive:
- create a cabal project called `yoursurname1`
- make sure the program Just Works upon typing `cabal run`
- package the project using `cabal sdist` and upload the solution to Moodle
  - if you do not have Moodle access, send it to me for evaluation via e-mail

## Evaluation

You are not supposed to provide super-optimized code and reasonable deviations from the assignment (esp. if creative in an interesting direction) are perfectly OK.

On the other hand, try to produce nice, well-arranged, readable and preferably _short_ program code. In particular,
- do not use recursion manually to solve everything but use higher-order functions (`map`, `zip`, `foldr`, `zip`, ...).
- use the fact that haskell is lazy and memory allocation is basically free; in result you can efficiently use nice constructions that would be "too wasteful" in other languages
  - For example, a sufficient way to change a list element at a certain index is to first add integer indexes (instead of `[Pixel]` you make `[(Int, Pixel)]`), make the change at the given index with `map` (possibly doing a few other operations on the indexed list right away), and then drop the indexes (getting back the `[Pixel]`).
- attempt to remove any code that looks even a tiny little bit repetitive
