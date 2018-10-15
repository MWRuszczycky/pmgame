{-# LANGUAGE TemplateHaskell #-}
module Model.Types
    ( Direction     (..)
    , Fruit         (..)
    , FruitName     (..)
    , Game          (..)
    , GameSt        (..)
    , Ghost         (..)
    , GhostName     (..)
    , GhostState    (..)
    , HighScore     (..)
    , Items         (..)
    , Message       (..)
    , Maze          (..)
    , MazeString    (..)
    , Mode          (..)
    , Name          (..)
    , PacMan        (..)
    , Point         (..)
    , Score         (..)
    , Tile          (..)
    , Time          (..)
    , TimeEvent     (..)
    -- Lenses for Game
    , dtime
    , fruit
    , ghosts
    , items
    , highscores
    , hsedit
    , level
    , maze
    , mode
    , msg
    , npellets
    , oneups
    , pacman
    , pwrmult
    , pwrtime
    , rgen
    , time
    -- Lenses for PacMan
    , pdir
    , ppos
    , pstrt
    , ptlast
    -- Lenses for Ghost
    , gdir
    , gname
    , gpathback
    , gpos
    , gstate
    , gstrt
    , gtlast
    -- Lenses for Items
    , fruits
    , gstscores
    , pellets
    , ppellets
    -- Lenses for Fruit
    , fdelay
    , fduration
    , fname
    , fpos
    ) where

import qualified Data.Matrix as M
import qualified Data.Text   as Txt
import Brick.Widgets.Edit           ( Editor     )
import System.Random                ( StdGen     )
import Lens.Micro.TH                ( makeLenses )

---------------------------------------------------------------------
-- Time management

type Time = Int

data TimeEvent = Tick Time deriving ( Show )

---------------------------------------------------------------------
-- Positioning and movement of movabel characters

data Direction = North | South | East | West deriving ( Show, Eq )

type Point = (Int, Int)

---------------------------------------------------------------------
-- Player management

data PacMan = PacMan { _pdir   :: Direction          -- Current direction
                     , _ppos   :: Point              -- Current position
                     , _pstrt  :: (Point, Direction) -- Initial pos. & dir.
                     , _ptlast :: Time               -- Time of last move
                     } deriving ( Show )

makeLenses ''PacMan

---------------------------------------------------------------------
-- Ghost management

-- | Normal ghosts can eat Pac-Man, Edible ghosts can be eaten by
-- Pac-Man, EyesOnly ghosts have been eaten and need to regenerate.
data GhostState = Normal | Edible | EyesOnly deriving (Show, Eq)

data GhostName = Blinky | Pinky | Inky | Clyde deriving (Show, Eq, Ord)

data Ghost = Ghost { _gname     :: GhostName          -- Name for the ghost
                   , _gdir      :: Direction          -- Current direction
                   , _gpos      :: Point              -- Current position
                   , _gstrt     :: (Point, Direction) -- Initial pos. & dir.
                   , _gstate    :: GhostState         -- Ghost state
                   , _gtlast    :: Time               -- Time of last move
                   , _gpathback :: [Point]            -- Path back to start
                   } deriving ( Show )

instance Eq Ghost where
    (==) g1 g2 = _gname g1 == _gname g2

instance Ord Ghost where
    compare g1 g2 = compare ( _gname g1 ) ( _gname g2 )

makeLenses ''Ghost

---------------------------------------------------------------------
-- Messages

-- |The first parameter is the message to display and the second is
-- the time remaining for its continued display, assuming no other
-- message is generated.
data Message = Message String Time | NoMessage deriving (Show, Eq)

---------------------------------------------------------------------
-- Fruit, item and score management

type Score = Int

type HighScore = (String, Score)

data FruitName = Cherry
               | Strawberry
               | Orange
               | Apple
               | Melon
               | Galaxian
               | Bell
               | Key
               deriving (Show, Eq)

-- |The number of pellets and power pellets eaten is self-
-- explanatory. Ghosts scores are tracked as the number of ghosts
-- eaten at each point level. Fruit scores are tracked as names of
-- the fruits eaten and their counts.
data Items = Items { _pellets   :: Int                -- Normal pellets eaten
                   , _ppellets  :: Int                -- Power pellets eaten
                   , _gstscores :: [(Score, Int)]     -- Score from ghosts
                   , _fruits    :: [(FruitName, Int)] -- Score from fruits
                   } deriving ( Show )

data Fruit = Fruit { _fname     :: FruitName    -- Name for the fruit
                   , _fduration :: Time         -- How long fruit lasts
                   , _fdelay    :: Time         -- Time before it appears
                   , _fpos      :: Point        -- Where the fruit appears
                   } deriving ( Show )

instance Eq Fruit where
    (==) f1 f2 = _fname f1 == _fname f2

makeLenses ''Items
makeLenses ''Fruit

---------------------------------------------------------------------
-- Maze management

-- |Raw string of ascii characters that represents all elements in
-- the maze.
type MazeString = String

-- |Interpretted maze of renderable tiles.
type Maze = M.Matrix Tile

-- |Most of these are self-explanatory. However, the Warp tile also
-- specifies the point where the warp leads to and the direction you
-- have to go in from the Warp tile to activate the warp. OneWays can
-- only be entered and exited in a single direction. OneWays are used
-- to prevent ghosts from rentering the starting ghost box.
data Tile = Player
          | Empty
          -- Pellets
          | Pellet
          | PwrPellet
          -- Ghosts
          | NormalGhost GhostName
          | BlueGhost
          | WhiteGhost
          | GhostEyes
          -- Fruit
          | FruitTile FruitName
          -- Maze walls
          | Wall Txt.Text
          | OneWay Direction
          -- Warp tiles
          | Warp Direction Point
          deriving (Show, Eq)

---------------------------------------------------------------------
-- Managing the game state

type GameSt = Either String Game

-- |Names for named widgets as required for text editors.
data Name = HighScoreEdit deriving (Show, Eq, Ord)

-- |After eating a power pellet during regular (Running) play mode,
-- the mode switches to PwrRunnig t, where t is the time remaining
-- in the powered mode.
data Mode = StartScreen       -- Play has not begun
          | Running           -- Normal play mode
          | PwrRunning Time   -- Play mode after eating a power pellet
          | GameOver          -- All oneups have have been used
          | NewHighScore      -- Game over with new high score
          | LevelOver         -- Current level has been completed
          | ReplayLvl         -- Player was captured and about to try again
          | Paused Mode       -- Game is paused
          deriving ( Show, Eq )

data Game = Game { _maze       :: Maze        -- Level maze
                 , _items      :: Items       -- Summary of items and score
                 , _pacman     :: PacMan      -- Player
                 , _ghosts     :: [ Ghost ]   -- List of ghosts
                 , _fruit      :: Maybe Fruit -- Fruit for the level
                 , _rgen       :: StdGen      -- Standard generator
                 , _npellets   :: Int         -- Pellets remaining in level
                 , _oneups     :: Int         -- Oneups remaining
                 , _mode       :: Mode        -- Current game mode
                 , _level      :: Int         -- Current level number
                 , _pwrtime    :: Time        -- Duration of power pellets
                 , _time       :: Time        -- Current in-game time
                 , _dtime      :: Time        -- Time since last update
                 , _pwrmult    :: Int         -- Score multiplier for ghost
                 , _msg        :: Message     -- In-game message
                 , _highscores :: [HighScore] -- Current high scores
                 , _hsedit     :: Editor String Name -- For naming high scores
                 } deriving ( Show )

makeLenses ''Game
