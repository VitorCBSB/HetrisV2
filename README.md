# Hetris

A [guideline](https://tetris.fandom.com/wiki/Tetris_Guideline) compliant (as far as I can tell) Tetris clone made with [SDL2](https://hackage.haskell.org/package/sdl2).

Initially the game was going to be implemented with [Gloss](https://hackage.haskell.org/package/gloss), which is a wonderful library, I really like it. The main issue I ran into is that Gloss programs, no matter how simple or complex, use _a lot_ of processing, literally making my CPU's fan spin hard. I don't know if that's a Windows thing or whatnot, but I ended up going with SDL2, with which I can avoid this issue.

This is called `V2` because the first version was my Gloss attempt.

## Compiling and Executing

`stack build` should do it! I'm not sure how to compile this project with `cabal` as I don't really use it, pull requests are welcome though!

To execute, just run `stack exec HetrisV2-exe`.

Make sure SDL2 is installed on your machine or compilation will fail. To do so on Windows, I used [MSYS2](https://www.msys2.org/) as it felt like the easiest way.

## Controls

Since I don't have a _Help_ screen yet, they are as follows (taken from the [guideline](https://tetris.fandom.com/wiki/Tetris_Guideline)):

- Up arrow and X: rotate 90° clockwise
- Space: hard drop
- Shift and C: hold
- Ctrl and Z: rotate 90° counterclockwise
- Esc and F1: pause
- Left and right arrows: movement
- Down arrow: soft drop and lock delay cancel.

## Missing Features

The game is still not completely implemented, a few things are missing from the guideline, specifically:

- _Korobeiniki_ (and other music) while the game runs.
- Different modes, like _marathon_ and _ultra_ modes.
- A main menu from which you can customize things and choose the aforementioned modes.
- Polishing in general.

Feel free to open issues (or PRs) about any non-compliance you find.

## File Organization

At the top level, the directories are `app`, which contains the source code for the game, and `assets`, which has the images, sounds and fonts the game uses.

### Code Architecture

The `Types` module contains all relevant types used for the game. If you want to know what a specific type looks like, that's very likely the place to look.

As for the actual code organization, as I mentioned before, I really like [Gloss's](https://hackage.haskell.org/package/gloss) API. So much so, in fact, that I made something similar to its `play` function and adapted it to be used with SDL2. It's called `mainLoop` (located in the `UtilsSDL` module). For those who are not familiar with that, it basically means that our `mainLoop` takes a bunch of arguments, the most important of which are:

- An input function that changes the state of our game in response to things like keyboard presses and mouse movement, with the type: `SDL.EventPayload -> world -> IO world`
- A tick function which has a delta time argument, which changes the state of our game in response to time, with the type: `Double -> world -> IO world`
- A render function, responsible for printing the current state of our game to the screen, with the type: `SDL.Renderer -> world -> IO ()`

`world` in that context is a type variable. In the case of our game `world = MainState` (as defined in the `Types` module). Their definitions can be found inside the `Main` module.

Speaking of `MainState`, it contains all assets loaded at startup as well as a sum type that controls the current "context" of the game (I called it `mainPhase` but it probably deserves a more fitting name). For instance, this is what it looks like:

```
data MainStatePhase
  = CountingDown CountingDownState
  | Game GameState
  | Paused PauseState
```

The important one in there is `Game`, that's when our game is actually running. The other two are related to the pause function and game start. Each of those has its own module for its set of `(input, tick, render)` functions (`CountingDown`, `Game` and `Paused` respectively).

The idea is that as the game grows, more entries will be added to `MainStatePhase`. For example, when a main menu gets added, a new entry will show up there for it. The same thing for a _Help_ screen or a _Credits_ screen and so on. Each of these will have their own set of `(input, tick, render)` functions.

The actual game rules are pretty complex, so it's probably not worth explaining them here (check the [guideline](https://tetris.fandom.com/wiki/Tetris_Guideline) for more detailed information on that).

### Assets

Images are all .bmp, sounds are all .wav and fonts, .tff. That's about it.
