{-# LANGUAGE OverloadedStrings #-}
module Loading
    ( advanceLevel
    , startNewGame
    , levels
    ) where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Model.Types as T
import Brick.Widgets.Edit           ( editor                )
import Data.List                    ( find, sort, sortOn    )
import Lens.Micro                   ( (&), (^.), (.~), (%~) )
import System.Random                ( randomR, StdGen       )
import Model.Utilities              ( fruitDuration
                                    , newMessage
                                    , powerDuration
                                    , tickPeriod
                                    , toMicroSeconds        )
import Model.Types                  ( Direction     (..)
                                    , Fruit         (..)
                                    , FruitName     (..)
                                    , Game          (..)
                                    , GameSt        (..)
                                    , Ghost         (..)
                                    , GhostName     (..)
                                    , GhostState    (..)
                                    , Items         (..)
                                    , Maze          (..)
                                    , Mode          (..)
                                    , Name          (..)
                                    , PacMan        (..)
                                    , Point         (..)
                                    , Tile          (..)
                                    , Time          (..)    )

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Pure functions for loading and transitioning between levels

-- |Raw string of ascii characters that represents all elements in
-- the maze.
type MazeString = String

-- |Raw MazeString where each ascii charcter has been indexed by the
-- row and column it represents in the level maze.
type IndexedMaze = [(Point, Char)]

-- =============================================================== --
-- List of levels and associated maze files

levels :: [ (Int, FilePath) ]
levels = [ ( -1, "levels/classicMaze1-testing1.txt" )
         , ( 1, "levels/classicMaze1.txt"   ) ]

-- =============================================================== --
-- Game initialization and level transitioning

startNewGame :: StdGen -> Int -> MazeString -> GameSt
startNewGame r0 lvl s = do
    xs   <- indexMazeString s
    m    <- loadMaze xs
    pman <- loadPacMan xs
    gsts <- mapM ( loadGhost xs ) "pbic"
    (mbFruit, r1) <- loadFruit r0 lvl xs
    return Game { _maze       = m
                , _items      = Items 0 0 [] []
                , _rgen       = r1
                , _pacman     = pman
                , _ghosts     = sort gsts
                , _fruit      = mbFruit
                , _mode       = Running
                , _level      = lvl
                , _npellets   = countPellets xs
                , _oneups     = 0 -- 3
                , _time       = 0
                , _pwrmult    = 2
                , _dtime      = 0
                , _pwrtime    = powerDuration lvl
                , _msg        = newMessage "Ready!"
                , _highscores = reverse . sortOn snd $ [("My Dog", 10)]
                , _hsedit     = editor HighScoreEdit ( Just 1 ) "Your Name"
                }

advanceLevel :: Game -> MazeString -> GameSt
advanceLevel gm s = do
    let nxtLvl = succ $ gm ^. T.level
    nxtGame <- startNewGame ( gm ^. T.rgen ) nxtLvl s
    return $ nxtGame & T.items   .~ ( gm ^. T.items  )
                     & T.oneups  .~ ( gm ^. T.oneups )
                     & T.time    .~ ( gm ^. T.time   )

-- =============================================================== --
-- Parsing level files

---------------------------------------------------------------------
-- Utilities

indexMazeString :: MazeString -> Either String IndexedMaze
-- ^Index a MazeString to get an IndexedMaze string making sure that
-- it is rectangular.
indexMazeString s
    | isRect    = Right . zip indxs . concat $ ss
    | otherwise = Left "Maze is not rectangular"
    where ss     = lines s
          nr     = length ss
          nc     = length . head $ ss
          isRect = all ( (== nc). length ) ss
          indxs  = [ (r,c) | r <- [1..nr], c <- [1..nc] ]

getDims :: IndexedMaze -> (Int, Int)
-- ^Rows and columns of the maze that an IndexedMaze represents.
getDims xs = (maximum rs, maximum cs)
    where (rs,cs) = unzip . fst . unzip $ xs

countPellets :: IndexedMaze -> Int
-- ^Get number of pellets and power pellets from an IndexedMaze.
countPellets = length . filter isPellet . snd . unzip
    where isPellet x = x == '.' || x == '*'

readPosition :: IndexedMaze -> Char -> Either String Point
-- ^Read the row and column of the first instance of a character from
-- an IndexedMaze string.
readPosition xs x =
    case find ( (== x) . snd )  xs of
         Nothing     -> Left $ "Cannot find '" ++ [x] ++ "' in maze"
         Just (p, _) -> Right p

---------------------------------------------------------------------
-- Loading the player from the IndexedMaze.

loadPacMan :: IndexedMaze -> Either String PacMan
-- ^Read and initialize the player according to the IndexedMaze.
loadPacMan xs = do
    pos    <- readPosition xs 'P'
    return PacMan { _pdir   = West
                  , _ppos   = pos
                  , _pstrt  = (pos, West)
                  , _ptlast = 0
                  }

---------------------------------------------------------------------
-- Loading the ghosts from the IndexedMaze.

loadGhost :: IndexedMaze -> Char -> Either String Ghost
-- ^Read and intialize a ghost according to the IndexedMaze.
loadGhost xs c = do
    pos    <- readPosition xs c
    (n, d) <- readGhost c
    return Ghost { _gname     = n
                 , _gdir      = d
                 , _gpos      = pos
                 , _gstrt     = (pos, d)
                 , _gstate    = Normal
                 , _gtlast    = 0
                 , _gpathback = []
                 }

readGhost :: Char -> Either String (GhostName, Direction)
-- ^Helper function for initializing each ghost by name given its
-- ascii representation in a MazeString.
readGhost 'p' = Right ( Pinky,  East  )
readGhost 'b' = Right ( Blinky, West  )
readGhost 'i' = Right ( Inky,   West  )
readGhost 'c' = Right ( Clyde,  North )
readGhost x   = Left $ "Character '" ++ [x] ++ "' not recognized as ghost"

---------------------------------------------------------------------
-- Loading the fruit from the IndexedMaze.
-- Fruit loading has the following random components:
-- 1. The name of the fruit, or no fruit at all, which also depends
--    on the level.
-- 2. The position of the fruit, which can be any position marked
--    with an '?' in the MazeString.
-- 3. The delay time before the fruit appears.
-- The duration of time that a fruit can be captured is not random.

loadFruit :: StdGen -> Int -> IndexedMaze -> Either String (Maybe Fruit, StdGen)
-- ^This is the entry function. It returns a Nothing value for the
-- fruit if no fruit is to appear in the current level.
loadFruit r0 lvl xs = case getFruit r0 lvl xs of
                           Nothing      -> Right (Nothing, r0)
                           Just (f, r1) -> Right (Just f, r1)

getFruit :: StdGen -> Int -> IndexedMaze -> Maybe (Fruit, StdGen)
-- ^This is the helper function that actually loads the fruit.
getFruit r0 lvl xs = do
    (p,  r1) <- getFruitPosition r0 xs
    (fn, r2) <- getFruitName r1 lvl
    let (t0, r3) = getFruitDelay r2
        dt       = fruitDuration fn
    return ( Fruit fn dt t0 p, r3 )

getFruitPosition :: StdGen -> IndexedMaze -> Maybe (Point, StdGen)
-- ^Draw a random position for the fruit selecting from all positions
-- in the IndexedMaze that correspond to a '?' character.
getFruitPosition r0 xs
    | null ps   = Nothing
    | otherwise = Just ( ps !! k, r1 )
    where ps     = [ p | (p, x) <- xs, x == '?' ]
          (k,r1) = randomR (0, length ps - 1) r0

getFruitName :: StdGen -> Int -> Maybe (FruitName, StdGen)
-- ^Draw which fruit will appear in the level or whether no fruit
-- will appear at all (10% chance). The probabilities are inversely
-- proportional to points each fruit is worth.
getFruitName r0 lvl
    | k < 480 && lvl > 0 = Just ( Cherry,     r1 )
    | k < 640 && lvl > 1 = Just ( Strawberry, r1 )
    | k < 736 && lvl > 2 = Just ( Orange,     r1 )
    | k < 805 && lvl > 3 = Just ( Apple,      r1 )
    | k < 843 && lvl > 4 = Just ( Melon,      r1 )
    | k < 877 && lvl > 5 = Just ( Galaxian,   r1 )
    | k < 893 && lvl > 6 = Just ( Bell,       r1 )
    | k < 900 && lvl > 7 = Just ( Key,        r1 )
    | otherwise = Nothing
    where (k, r1) = randomR (0, 999 :: Int) r0

getFruitDelay :: StdGen -> (Time, StdGen)
-- ^Draw the time delay befor the fruit will appear in the level.
getFruitDelay = randomR (tmin, tmax)
    where tmin = toMicroSeconds 5
          tmax = toMicroSeconds 25

---------------------------------------------------------------------
-- Loading the actual maze.
-- Only the fixed maze tiles such as walls, pellets, warps and
-- oneways are loaded here. The player, ghosts and fruit are managed
-- and loaded separately, and the final maze with all tiles in place
-- is only generated right before rendering it to the screen.

loadMaze :: IndexedMaze -> Either String Maze
-- ^Entry function for generating the maze.
loadMaze [] = Left "No maze string provided."
loadMaze xs = M.fromList nr nc <$> mapM (readTile xs) xs
    where (nr,nc) = getDims xs

readTile :: IndexedMaze -> (Point, Char) -> Either String Tile
-- ^Convert ascii characters to fixed maze tiles. Everything else in
-- the IndexedMaze is interpretted as an empty tile.
readTile _  (_, '.') = Right Pellet
readTile _  (_, '*') = Right PwrPellet
readTile xs (p, 'w') = resolveWarp xs p
readTile _  (_, '^') = Right . OneWay $ North
readTile _  (_, 'v') = Right . OneWay $ South
readTile _  (_, '<') = Right . OneWay $ West
readTile _  (_, '>') = Right . OneWay $ East
readTile xs (p, '|') = Right . resolveWall xs p $ '|'
readTile xs (p, '=') = Right . resolveWall xs p $ '='
readTile _  _        = Right Empty

resolveWarp :: IndexedMaze -> Point -> Either String Tile
-- ^Setup warps for the maze. Note that only one pair of warps is
-- allowed in each maze that warps must always be paired.
resolveWarp xs (r,c)
    | r == 1    = Warp North <$> wLoc
    | c == 1    = Warp West  <$> wLoc
    | r == nr   = Warp South <$> wLoc
    | c == nc   = Warp East  <$> wLoc
    | otherwise = Left "Incorrect placement of warp tile"
    where (nr, nc) = getDims xs
          wLoc = case filter ( \ (p,x) -> x == 'w' && p /= (r,c) ) xs of
                      []        -> Left "Cannot find matching warp tile"
                      w:[]      -> Right . fst $ w
                      otherwise -> Left "Too many warp tiles"

isWallChar :: Char -> Bool
-- ^Defines what is a valid ascii representation of a wall character.
-- '|' denotes a vertical wall character and '=' denotes a horizontal
-- wall character.
isWallChar x = x == '|' || x == '='

resolveWall :: IndexedMaze -> Point -> Char -> Tile
-- ^Resolve how ascii characters representing walls should be linked.
resolveWall xs (r,c) x
    | nw && sw && ww && ew = Wall "╬"
    | nw && sw && ww       = Wall "╣"
    | nw && ww && ew       = Wall "╩"
    | nw && sw && ew       = Wall "╠"
    | sw && ww && ew       = Wall "╦"
    | nw && ww             = Wall "╝"
    | nw && ew             = Wall "╚"
    | sw && ww             = Wall "╗"
    | sw && ew             = Wall "╔"
    | sw || nw             = Wall "║"
    | otherwise            = Wall "═"
    where nw  = verticalLink x   . lookup (r-1, c) $ xs
          sw  = verticalLink x   . lookup (r+1, c) $ xs
          ww  = horizontalLink x . lookup (r, c-1) $ xs
          ew  = horizontalLink x . lookup (r, c+1) $ xs

verticalLink :: Char -> Maybe Char -> Bool
-- ^Is a character a wall character and linked above or below to
-- another wall character that may not exist. Stacked wall characters
-- running parallel horizontally should not be linked.
verticalLink _   Nothing    = False
verticalLink '|' (Just '|') = True
verticalLink x   (Just y  ) = isWallChar x && isWallChar y && x /= y

horizontalLink :: Char -> Maybe Char -> Bool
-- ^Is a character a wall character and linked left or right to
-- another wall character that may not exist. Side-by-side wall
-- characters running parallel vertically should not be lined.
horizontalLink _   Nothing    = False
horizontalLink '=' (Just '=') = True
horizontalLink x   (Just y  ) = isWallChar x && isWallChar y && x /= y
