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

## Things to do

* Improve the entry point to handle options.
* Allow changes to terminal settings.
* Make the player die more gracefully.
* Include an "about" screen with version information.
* Improve level progression and maze layouts.
* Improve installation.
* Write a better README.
