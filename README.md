# pmgame

Terminal-based game for Linux similar to Pac-Man and written entirely in Haskell using [Brick](https://hackage.haskell.org/package/brick).

This is still a work in progress, but the basic game is working.

## Downloading and compiling

The program uses the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/). Clone the repository and compile with
```sh
git clone https://github.com/MWRuszczycky/pmgame.git
cd pmgame
stack build
```
To run the game use
```sh
stack exec pmgame
```
Right now things are not setup for a proper installation and only for testing, because the levels (and eventually high scores) are stored as separate files in their own directory (`levels/`), and there is only one real level so far. So, it needs to be run from within the cloned repository. This will be fixed once other issues are taken care of.

## Controls

Use the arrow keys to change direction. Use `Esc` to quit the game at any time.

## Things to do

* Improve parameters for the fruit and add the full list (right now they are just for testing purposes).
* Add display for acquired fruit rather than just displaying "fruit!".
* Add display for control information.
* Improve the entry point and add a start screen.
* Add game pausing.
* Add recording and loading of high scores (right now it just redisplays the current score).
* Add dedicated data types for ghost and fruit names rather than using tiles.
* Allow capture of multiple ghosts simultaneously.
* Refactor the `Loading` module.
* Refactor the fruit updating function (it's really ugly right now).
* Make more levels and improve installation.
* Write a better README and add images or an animated gif.
