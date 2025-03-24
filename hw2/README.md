# Assignment 2 -- functions with variables

Let's make a small language that would help us with math. On the practicals, we
made a tiny language that can process simple mathematical expressions such as
`(1+2)*3`. The task of assignment 2 is to extend this language with several new
constructions, and implement several algorithms that work with the code.

Briefly, we are going to process a small mini-language where people can define
named functions with parameters, all of which compute some kind of mathematical
expression.

As an example, one could write the following program (which calculates
solutions to a quadratic equation):
```
discriminant(a,b,c) = b*b - 4*a*c;
solutionA(a,b,c) = (-b+sqrt(discriminant(a,b,c)))/(2*a);
solutionB(a,b,c) = -(b+sqrt(discriminant(a,b,c)))*0.5/a;
sumOfSolutions(a,b,c) = solutionA(a,b,c) + solutionB(a,b,c);
```

Formally, the program contains:

- Definitions of functions, each consisting of a function name (e.g.,
  `discriminant`), parentheses with comma-separated parameters (e.g., `(a, b,
  something)`), equal sign (`=`), the body of the function, and a semicolon
  (`;`) at the end.
- Names of functions and parameters consist of sequences alphabetic letters
  (both upper and lower case).
- Function bodies consist of usual mathematical notation with infix operators
  and numbers:
  - parentheses (`(` and `)`),
  - numbers formatted as integers (`123`) or decimals (`12.34`)
  - variables that refer to function parameters (`a`, `bcd`, ...)
  - infix operators with assigned priorities as follows (from highest to lowest
    priority):
    - unary `-` (E.g., `-5`. Since the priority is high, `-5-5` should evaluate
      as `(-5)-5), i.e., `-10` instead of `0`.)
    - multiplication and division with ` * ` and ` / `
    - addition and subtraction with ` + ` and ` - `
    You can choose any direction of associativity of the operators; in
    particular it does not matter to the assignment if `1+2-3` parses as
    `(1+2)-3` or `1+(2-3)`; but it should parse consistently in the chosen way.
  - function "calls", which consist of a function name followed by open
    parenthesis (`(`), list of comma-separated argument expressions, and a
    closed parenthesis (`)`)
    - argument list may be empty; at that point the function is essentially a
      constant
    - function names are distinct from "variable names", and it is impossible
      to pass in a function via argument or return it. In particular, `a(x) =
      x+1; f(a) = a(a);` is valid code because the function name `a` and
      variable name `a` are referring to something differrent.
    - several simple functions are "built in", these include `sqrt`, `abs`,
      `sin`, `cos` and `tan`.

## Task 1: Parse and format

Write a program called `pretty_functions` which reads the function definitions
as above on the standard input, and prints them nicely formatted to the
standard output. If parsing of the input fails for whatever reason, print out
any suitable error message and terminate the program.

At the barest minimum, the formatting must be able to normalize the spaces in
the formulas, and avoid printing of unnecessary parentheses. For example, this
function:

```
f(a,b,    c) = (a+((b))+c)*((a   -    b)-     c);
```
...should get formatted to something like:
```
f(a, b, c) = (a + b + c) * (a - b - c);
```
(The above example assumes that `-` is left-associative; the interpretation in
your program may differ.)

Similarly, parentheses in this function:
```
f(x,y) = (a*b)+(a/b);
```
...should be removed:
```
f(x, y) = a * b + a / b;
```

To simplify testing, you may use [`getArgs :: IO
[String]`](https://hackage.haskell.org/package/base-4.21.0.0/docs/System-Environment.html#v:getArgs)
to read a filename that was passed to the program via a command line argument,
and read the input from the given file.

Your solution must define a suitable datatype or (or at least type aliases) for
internal representation of the program code. You must use some
parser-combinator library to implement the parsing; preferably this would be
[megaparsec](https://hackage.haskell.org/package/megaparsec-9.7.0/docs/Text-Megaparsec.html),
but any other library with a similar interface (e.g.,
[attoparsec](https://hackage.haskell.org/package/attoparsec-0.14.4/docs/Data-Attoparsec-ByteString.html),
or even your own) is OK.

For a quick start, follow the example in [`parser.hs`](./parser.hs).

### Bonus 1: Indenting (optional)

The formatting should be able to also break very long function definitions to
multiple lines; for example this absolutely ugly function:
```
g(a) = (a+a+a+a+a) * (a+a+a+a+a) * (a+a+a+a+a) * (a+a+a+a+a) * (a+a+a+a+a) * (a+a+a+a+a) * (a+a+a+a+a) * (a+a+a+a+a) * (a+a+a+a+a) * (a+a+a+a+a);
```
is much less ugly if formatted like:
```
g(a) = (a + a + a + a + a)
       * (a + a + a + a + a)
       * (a + a + a + a + a)
       * (a + a + a + a + a)
       * (a + a + a + a + a)
       * (a + a + a + a + a)
       * (a + a + a + a + a)
       * (a + a + a + a + a)
       * (a + a + a + a + a)
       * (a + a + a + a + a);
```

You can choose whatever formatting and indenting scheme you like -- anything
that looks consistent and nice is OK.

Ideally, you should use a suitable pretty-printing library, such as
[pretty](https://hackage.haskell.org/package/pretty).

### Bonus 2: Compiling to Scheme (optional)

Write a program `schemify_functions` that prints out nicely-formatted
[Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)) code that
implements the functions given on input.

Notably, Scheme does not know infix operators, and all function and operator
calls are written in "prefix" notation with parentheses around. E.g., `1+2` is
simply written as `(+ 1 2)`.

For example, the program from above:
```
discriminant(a,b,c) = b*b - 4*a*c;
solutionA(a,b,c) = (-b+discriminant(a,b,c))/(2*a);
solutionB(a,b,c) = -(b+discriminant(a,b,c))*0.5/a;
sumOfSolutions(a,b,c) = solutionA(a,b,c) + solutionB(a,b,c);
```
...may be translated to Scheme as:
```scm
(define (discriminant a b c) (- (* b b)
                                (* 4 a c)))
(define (solutionA a b c) (/ (+ (- b)
                                (discriminant a b c))
                             (* 2 a)))
(define (solutionB a b c) (/ (* (- (+ b (discriminant a b c)))
                                0.5)
                             a))
(define (sumOfSolutions a b c) (+ (solutionA a b c)
                                  (solutionB a b c)))
```
Note the above example also includes "nice" formatting, which is great but not
mandatory to get the bonus point.

You can try to run your translated programs online at https://try.scheme.org/ .

## Task 2: Check validity of the definitions

Function definitions may be invalid for many reasons; e.g., they may use
undefined variables, unknown functions, or recurse infinitely.

Make a program `check_functions` that reads the function definitions on input,
and prints out information about the following issues that occur in the code:

- duplicate definitions, such as in `f(x) = 1; f() = 2;`
- name conflicts with built-in functions, such as in `sin(x)=x+1;`
- undeclared variable names, such as in `f(x) = x + y;`
- names of unknown functions, such as in `f(x) = x + something();`
  - note that the built-in functions (`sqrt` and others) are not unknown and
    should not generate an error.

The error messages do not need to be complicated; and the single ("first")
error reported for each program run should be sufficient.

### Bonus 3: Finding recursion (optional)

Because our language does not have a suitable `if` construction, there is no
way to use recursion without hitting infinity.

To prevent this, also check that there is indeed no recursion in the
definitions. This is best done by imagining the function definitions as graph
vertices, function calls as graph edges, and checking that the program graph
contains no cycles.

For example, both of the following programs should trigger the error:
- `a(x) = a(x)+1;`
- `f(x) = g(x); g(x) = f(-x);`

### Bonus 4: Source locations (optional, hard)

Make sure all parsing errors *and* the errors detected in the programs are
printed out with exact error location in the input file (e.g., the program
prints `Input line 10 column 15: unknown identifier 'asd'`.)

Note that there is specific functionality in `megaparsec` to support this:

- https://hackage.haskell.org/package/megaparsec-9.7.0/docs/Text-Megaparsec.html#v:getSourcePos
- https://hackage.haskell.org/package/megaparsec-9.7.0/docs/Text-Megaparsec-Pos.html#v:sourcePosPretty

## Task 3: Evaluate the functions

Write a program `evaluate_function` that additionally gets the following
command line arguments (accessible via [`getArgs :: IO
[String]`](https://hackage.haskell.org/package/base-4.21.0.0/docs/System-Environment.html#v:getArgs)):

- path to a file with function definitions (e.g., `documents/MyFunctions.txt`)
- function name (e.g., `sumOfSolutions`)
- values of parameters for the function (e.g., `1`, `0`, and `-1`)

With that, it simply evaluates the corresponding function as loaded from the
file and prints out the value of the result. You can assume all values can be
safely evaluated as `Float`s.

For example, if the quadratic-equation-solving example from above is saved in a
file `test.txt`, running the program as follows:
```
cabal run evaluate_functions -- test.txt sumOfSolutions 1 0 -1
```
...should simply print out:
```
0
```

### Bonus 5: Partial evaluation (optional)

Write yet another program `substitute_function` that takes the same arguments
as `evaluate_function`, but does not actually evaluate anything and only prints
out the final substituted formula. This formula contains only operators and
built-in functions calls (i.e., there are no variables or calls to user-defined
functions).

In particular, the output of `substitute_function` for the same input as given
to `evaluate_function` above might look like:
```
(-0+sqrt(0*0-4*1*(-1))/(2*1))+(-(0+sqrt(0*0-4*1*(-1))*0.5)/1)
```
