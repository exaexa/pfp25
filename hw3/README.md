
# Homework 3 -- icon.io

The main task of the last homework is to practice the networked concurrent
programming and synchronization primitives, such as
[Chan](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-Chan.html)
and
[MVar](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Concurrent-MVar.html).

As discussed at the lectures, Haskell run-time contains a standalone
implementation of file-descriptor polling, which relieves the programmers from
solving the usual trouble with blocking reds and writes to network sockets. In
result, concurrent programming (i.e., a "multi-threaded-like" programming that
only uses a single real OS thread) is remarkably easy, and you can implement it
simply using `forkIO`. The "lightweight" threads made by `forkIO` do not cost
practically anything, and you can use them to implement various complex
communicating and networking applications.

In other languages, making the real-time networking applications work right
requires quite a lot of asynchronous programming gymnastics -- typically, you
need to either explicitly run multiple (costly) OS threads, or implement the
complex polling yourself.

## Preliminaries

You are given a [simple implementation of a multiplayer icon editor
**server**](paintserver.hs). This server allows multiple users to edit a single
"drawing board" with a single picture over internet. For the purposes of this
homework, the board size is always exactly 32×32 pixels. You may find it
remarkably similar to what you did in homework 1.

The communication protocol used by the server is quite unsurprising: Clients
connect via a simple unencrypted TCP connection (just as you would for HTTP),
over which they send single-line text-board-editing commands to the server, and
receive single-line text responses that describe the changes of the board.

The clients may send the following text commands:
- `Transparent x y` tells the server to change the pixel in `x`-th column and
  `y`-th row (counted from zero) to the transparent "color"
- similarly, `Light x y` and `Dark x y` should change the pixel to the "light"
  and "dark" color shades.
- `Quit` is used to declare the disconnection of the client; upon receiving it,
  the server closes the connection
- `Poll` is used to ask for the contents of the drawing board; the server
  responds with a full description of the current state of the drawing board.
  (Because polling is a relatively costly operation, it is not recommended to
  run it very often. Technically, clients only need it once, to find the
  initial state of the board to display right after the connection.)

The server is able to send the following 3 message types:

- `Error` announces to the client that the previous command was not processed
  (e.g., because the server wasn't able to parse it)
- `Paper [...]` is an answer to `Poll`. The message contains a complete board
  state encoded in a string of length 1024 characters (32×32). The string
  contains either dots (`.`) that represent transparent pixels, circles (letter
  `o`) that represent dark pixels, and crosses (letter `x`) that represent
  light-colored pixels. The whole drawing board is encoded by consecutive lines
  from top, without any extra spaces or line separators. N.B., the message
  `Paper` may sometimes arrive even without the client explicitly asking for it
  via `Poll` -- the server uses this to announce changes without waiting for
  the clients to ask for them.
- `Transparent x y`, and similarly `Light` and `Dark` announce the change of a
  single pixel by any of the users.

You should be able to try the communication with the server manually, using
either `telnet` or netcat (`nc`). Assuming the above server
([`paintserver.hs`](paintserver.hs)) runs on your computer on the default port
(10042), this should work:

```
 ~ $ telnet localhost 10042
Trying ::1...
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
< Poll
> Paper ................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................
< Dark 0 0
> Dark 0 0
< Light 0 2
> Light 0 2
< Poll
> Paper o...............................................................x...............................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................
< Dark 2 0
> Dark 2 0
< Light 1 2
> Light 1 2
< Light 2 2
> Light 2 2
< Poll
> Paper o.o.............................................................xxx.............................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................
< Quit
Connection closed by foreign host.
 ~ $
```

The above example shows the process of a single client drawing a slightly
post-modern version of the bored face emoji, letting the server to transmit the
whole intermediate state using `Poll` a few times. For clarity, the above
description prefixed the lines sent from client to server by `<` ("coming in")
and the ones from server to client by `>` ("going out"). For completeness, the
resulting board might look roughly like this in 2D:

```
o.o............................
...............................
xxx............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
...............................
```

You can also use a [helper testing program](sendpixel.hs) that correctly
resolves the server address, connects to it, sends 2 commands (`Dark 0 0` and
`Poll`) and prints out any returning messages to the standard output. (Note the
program disconnects quite harshly by just closing the connection without
announcing closure by `Quit`; to which the server may respond by printing out
some complaints about unexpected client disconnects.)

## Your task

Take your implementation of icon-drawing program from task 1 (or the reference
solution that was supplied via Moodle) and add the support for collaborative
editing on the server.

The recommended implementation strategy is as follows:
- Implement the sending and receiving part as standalone "lightweight" threads,
  spawned off using `forkIO`. Use feasible synchronization primitives to
  connect these 2 threads to the rest of the program (such as IO-based
  references, channels, or locking variables). The primitives should allow the
  user-facing "display" part of the program to work completely independently
  (asynchronously) on the server communication. In particular,
  - If you used Gloss, any network communication running in the main thread
    will cause temporary freezing of the screen (because the redraw loop will
    wait for the networking operation to finish). That is better avoided.
  - If you used Brick, you additionally have to tell the Brick frontend to
    sometimes wake up and redraw itself (compared to Gloss, it only redraws
    itself when an user event changes the widget state, which (by default)
    doesn't include the networking events). To do that properly, you can make
    your own type of an event that pushes Brick to redraw -- ideally follow
    [the section about "Variable speed" in this
    tutorial](https://samtay.github.io/posts/introduction-to-brick).
  To get the networking setup running properly without unnecessary guesses and
  pitfalls, inspire yourself heavily the server setup in `paintserver.hs`
  (simply copying code from there is OK).
- Do not modify the drawing board yourself, not even as a reaction to your
  user's input -- simply display the version that the server sent to you. The
  only state that you manage directly yourself as a client is the "cursor
  position" (which you do not even announce to the server).
- In case of any user action, send the corresponding drawing command (or
  multiple commands, in case the user draws a bigger shape) to the server.
  Eventually, you'll get a message from the server that tells you exactly what
  to display. This will also include server's resolution of potentially
  conflicting edits from multiple clients (what if 2 people are editing one
  pixel at the same time?). Also, your program should not generally wait for
  the response, but should allow the user to run more actions (and only draw
  the updates as they arrive -- imagine that the server is lagging and your
  user wants to draw
  [this](https://www.redbubble.com/es/i/lamina-fotografica/Mona-Pixel-Pixelated-Mona-Lisa-de-Galiderath/16003491.6Q0TX).
- In the drawing program, preserve at least one kind of functionality for
  drawing larger objects (lines, rectangles, circles, inverse effect, or
  something other). Announce such larger changes to the server by dismantling
  them to a series of individual single-pixel operations.
- **There are multiple perils that will threaten the stability of your program,
  which stem from the fact that internet is a completely unreliable medium.**
  You are **not** required to manage any kind of the uncountable ways that the
  networking may fail in any way. This means that in case of networking issues
  such as connection loss and excessive latency, your program may simply crash.

### Details

- Tools for network communication can be found in module `Network.Socket`, for
  threads in `Control.Concurrent`, for inter-thread communication in
  `Control.Concurrent.MVar`, in `Control.Concurrent.Chan`, and some extra
  alternatives are present in package `stm`. It is highly recommended to get
  inspired by the example server code.
- Read the address and port of the server (the default one is `10042`) from the
  command line. The program should support position-independent arguments `-a
  <server-address>` and `-p <port>`, and complain in case the parameters are
  missing or bad.

## Bonus assignments

### Bonus 1 (optional but recommended)

Use the very nice library `optparse-applicative` to parse the command line
arguments.

For both parameters, also implement the "portable&long" variant (`--address`),
and print out a helpful listing of the command line arguments in case the user
makes an error (or asks for help using `--help`).

### Bonus 2 (optional, for typesystem fans)

Be super-cool and try to implement all local state modifications using a
`State` monad, with the state accessed via optics (e.g. via `Control.Lens`).

For example, the incoming events might be processed in a java-lookalike style
as follows:
```hs
handleCursorRight = runState $ do
  cursorX += 1
  cursorX . filtered (>31) .= 31
```

(The code increases the field `cursorX` in the state variable. Similarly, you
could write e.g. `cursorX . filtered (<31) += 1`.)

You may get quite creative in applying lenses to various other situations, esp.
the networked-update-handling code.

### Bonus 3 (optional, for packaging fans)

`cabal` offers the possibility to pack multiple executables into a single
package. Customize your `.cabal` file appropriately so that the programs
"sendpixel" and "paintserver" are distributed (and submitted) in the same
package along with your solution. See the [official Cabal
documentation](https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html#example-a-package-containing-executable-programs)
for details.

### Bonus 4 (optional, very hard)

Implement server-side support for commands `Nick some_artist_name` and
`CursorPos x y`, which the users may announce their names and cursor positions
to the server.

In response, implement messages `CursorAt some_artist_name x y` and `Lost
some_artist_name` that announce the names and current editing positions of
other artists to the clients. The client program should allow specifying the
artist name e.g. via a command line option.

Render cursors of all connected users in the GUI (in some reasonable way).

## Hints

- Your application should _only display the drawing board state reported by the
  server_. Do not try to implement any logic of merging user actions with
  asynchronous server updates -- it's too hard. You can simply trust the server
  to do it right for you.
- In case you'll have trouble with polling, add some simple functionality for
  refreshing (e.g., `F5`, which just sends a `Poll` message to the server).
- If this is the first networking application that you will implement, the
  network programming API (derived from [Berkeley
  sockets](https://en.wikipedia.org/wiki/Berkeley_sockets)) may shock you.
  Despite of that, most of the networking code can be successfully copied from
  the attached examples. The main difference between your program and the
  server code is that you will not use the "listening" combo
  `bind`-`listen`-`accept`, but only a single function
  [`connect`](https://hackage.haskell.org/package/network-3.1.1.0/docs/Network-Socket.html#v:connect)
  that manages the creation of TCP connection to the server address for you.
- **Do not parse the IP address from the command line manually.** (If you
  decide to do so, do not forget about the IPv6 support!) Instead you want to
  use
  [`getAddrInfo`](https://hackage.haskell.org/package/network-3.1.1.0/docs/Network-Socket.html#v:getAddrInfo).
- In some situations, you might want to use a stateful computation and IO at
  once (that may happen in some event-handling code -- some actions change the
  cursor position, some actions send messages to the server, some do both). In
  such case, use some viable combination of monad transformers; such as
  `type EventHandler a = StateT Board IO a` or so.
- **You will probably need some introspection for debugging various parts of
  your program**. To "cheat the IO" for debugging purposes, you may use the
  library
  [`Debug.Trace`](https://hackage.haskell.org/package/base-4.21.0.0/docs/Debug-Trace.html)
  that allows you to print debugging messages even from the functions that can
  not do IO.
