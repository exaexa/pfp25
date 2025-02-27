
# Homework 0: ASCII letter statistics

Write a program that reads the whole input from the user (on the standard input aka. `stdin`) and writes out the percentage of vowels (`aeiouy`) in all letters in the input.

Simplifications:
- If you do not want to care about exact floating-point computation of the percentages, you can truncate the percentage to whole percents.
- Only consider ASCII vowels listed above -- we do not need to care about Unicode letters such as `ěéáýụüöäȧ` and similar nuisances.
- You can assume that the input is short and fits into RAM without any problems.

Input example:
```
aaasssddd
    ---lalalali...
```

Expected output:
```
41%
```

Explanation: There are 7 vowels (6 `a`s and 1 `i`) and total 17 letters in the input. The percentage is around `41.176470588235294118`, so `41%` is a valid answer.

**Bonus assignment:** :trophy: The code for a simple program should be _short_. Try to use the least amount of code while preserving readability. Do not shorten variable names (all variable names count as 1 "item" -- we're trying to produce concise code, not play codegolf). Feel free to add your own datatypes to make the task more concise. (To win this, you may want to wait for lecture 2 or 3 for some Functor and Applicative tricks.)

## How-To

Because this is the first assignment, I'm adding a list of functions from the standard library that you will likely want to use. Documentation for all functions can be found at [hoogle](https://hoogle.haskell.org/); usually it also pays to explore a slight neighborhood of the entry in documentation, in order to find more interesting (possibly more suitable!) extra functions.

- [`elem`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:elem)
- [`isAlpha`](https://hackage.haskell.org/package/base/docs/Data-Char.html#v:isAlpha) from the library `Data.Char` can tell you if a given character is a letter
- [`getContent`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:getContents) for reading the whole input (it is an IO action that produces a `String`), or alternatively [`interact`](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:interact)
- [`putStrLn`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:putStrLn), [`show`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:show) or [`print`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:print) for printing and/or formatting the output
- [`fromIntegral`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:fromIntegral) for converting integers to anything, which may help especially if you want to compute a more precise floating-point percentage. For conversion back to integers, you may want to use [`truncate` or a similar function](https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:truncate).

### How to make a "project" ?

Haskell ecosystem has standardized a quite useful packaging system that is managed by program `cabal`. You can use it as follows:

#### 1. Initialization

Make a directory for a project, an run the following command in it:
```
cabal init --interactive
```
Cabal is going to ask you for some details about the project (whether it is a library or an executable, what is the license, name, synopsis, ...), after which it will make a minimal directory structure for the package. If you do not know answers for some of the questions, use the defaults -- they should generally work.

The project for the first homework has to be called `surname0` where you substitute `surname` for your lowercase surname. I.e., I would call mine `kratochvil0`.

All information about the project is saved in file `myprojectname.cabal` (in case of my homework zero, that would be `kratochvil0.cabal`). The interactive system is additionally going to ask you whether you want to leave useful beginner-friendly comments in the file; I recommend that you do that and read through the comments at least once to see all the possibilities:
```
Add informative comments to each field in the cabal file (y/n)? [default: n] y
```

#### 2. Programming

If the initialization went well, you should get a half-empty default source file called `Main.hs`. You can edit that with any editor you like. When you are happy with the edits, you can build and run the program using command `cabal run`. It is good to check the functionality without editing the file first -- the default `Main.hs` program will typically print out something like `Hello Haskell!`).

More Cabal commands:
- You can tell Cabal to "only build" the project without running with `cabal build` -- that is pretty good for fixing compile errors in your program.
- If you need non-standard libraries, you can add them using `cabal install libraryname`. For example, to run the demo from the slides that draws the rotating circles, you will probably need to run `cabal install gloss`. You will typically find more interesting packages and their names on Hoogle. If you need to use the other packages in your homework (or generally any project), do not forget to write them down in your `.cabal` file as dependencies.
- You can interactively play with any part of your program in an interpreter. Typing `cabal repl` is going to run GHCi with the correct environment that can re-run the code in your project. Using `:l` in the interpreter allows you to load any file from the project; and `import` will work with the project-internal modules.

With this in hand, you can write your solution to `Main.hs` and test its functionality.

#### 3. Submitting

When your project works, you can ask Cabal to make you a nice redistributable (and submittable) archive of your project with:
```
cabal sdist
```
This makes an archive called `packagename-version.tar.gz` (in case of my project, `kratochvil0-0.1.0.0.tar.gz`). Submit that file as your solution in Moodle. (Or send it to me via mail if you don't have Moodle access.)

Cabal misconfiguration can, in some cases, cause Cabal to **not** include your source code in the archive. If you're not sure, you can always check that the package builds again from unpacked source, or just manually verify the contents with `tar tzf yourfile.tar.gz`.

**Demo solution is available as an [archive suitable for submission](kratochvil0-0.1.0.0.tar.gz) and in an unpacked [version suitable for browsing](kratochvil0-0.1.0.0).** (The demo solution ofcourse doesn't solve the homework task -- that would be too easy, right?)

### Checking the solution before submission

It is always nice to get some assurance about the quality of your code using automatic means. For example, it is quite useful to switch on all compiler warnings -- these typically advice you against possibly fragile or otherwise problematic constructions that you used. You can do that by adding the right flags into the `executable` section in your project `.cabal` file:
```
executable surname0

  ...
  ghc-options: -Wall
  ...
```

Remember to force rebuilding of the whole project using `cabal clean` (which does the same thing as the more common `make clean`; moreover newer versions of Cabal will do that automatically if you change the build options).

You can check the package formatting with `cabal check`. The warnings there are more of the kind that you would have to solve if you were submitting your package for publication in Hackage repositories -- for the purposes of the homework you do not need to solve almost any of them.

You can have your source code "checked" for common deficiencies using a linter -- the most common is likely `hlint`. You can install that one using `cabal install hlint`. The installation takes some time (roughly 10 minutes) because it recompiles a large modified portion of GHC. The binary will be installed in a directory where the other Cabal-managed programs reside; on Unixes that is typically `~/.cabal/bin` (which you may add to your `PATH` for convenience). The source code can then be checked by running `~/.cabal/bin/hlint Main.hs` (or alternatively just `hlint Main.hs`, if you set up the path there).

To automatically format your source code, you may use some of the autoformatters. The "Standard" one is called `hindent`, but sometimes it may be too much "cutting-edge" for many new users (esp. regarding some relatively invasive choices of formatting style). Alternatively you can use e.g. [fourmolu](https://github.com/fourmolu/fourmolu). Both `hindent` and `fourmolu` are installed and executed just like `hlint`: Installation is done with `cabal install hindent`, and run e.g. `hindent Main.hs` or `fourmolu --mode inplace Main.hs`.
