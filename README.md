# pmgame

Terminal-based game for Linux similar to Pac-Man and written entirely in Haskell using [Brick](https://hackage.haskell.org/package/brick).

This is still a work in progress, but the basic game is working:

![pmgame demo](demos/demo1.gif)

## Downloading and compiling

Right now things are not set up for a proper installation and only for testing; however, the game can be compiled and run within the cloned repository. The program uses the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/). Clone the repository and compile with
```sh
git clone https://github.com/MWRuszczycky/pmgame.git
cd pmgame
stack build
```
To run the game use
```sh
stack exec pmgame
```

Right now the mazes and high scores are stored as separate files in their own directory (`dev/`). Eventually this will all be reorganized so that proper installations are possible.

## Controls

* Use the arrow keys, `awsd` (Querty) or `a,oe` (Dvorak) to change direction.
* Use `Esc` to quit the game at any time.
* Use `Space` to pause the game.

## Colors

When the game runs, it sets the `TERM` environment parameter to `xterm-256color`. If this causes a problem such as a message like
```sh
pmgame : setupTerm: Couldn't lookup terminfo entry "xterm-256color"
```
then you can try using a different terminal setting using the `--terminal` option. For example, try,
```sh
pmgame --terminal=xterm-16color
```
The game should still display correctly with only 16 colors; however, you will start to lose colors if you go below 16. For example, grey text will not show up and power pellets will not flash if you set `--terminal=xterm`, but everything else should still work.

## Things to do

* Improve the entry point to handle options.
* Make the player die more gracefully.
* Include an "about" screen with version information.
* Improve level progression and maze layouts.
* Improve installation.
* Write a better README.
