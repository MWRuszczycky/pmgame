{-# LANGUAGE TemplateHaskell #-}
module Model.Types
    ( Tile          (..)
    , Maze          (..)
    , Point         (..)
    , Game          (..)
    , GameSt        (..)
    , Ghost         (..)
    , Fruit         (..)
    , PacMan        (..)
    , Direction     (..)
    , TimeEvent     (..)
    , Items         (..)
    , Status        (..)
    , GhostState    (..)
    -- Lenses for Game
    , maze
    , items
    , pacman
    , ghosts
    , rgen
    , npellets
    , oneups
    , status
    , level
    , pwrtime
    , time
    , dtime
    , fruit
    , pwrmult
    , msg
    -- Lenses for PacMan
    , pdir
    , ppos
    , pstrt
    , ptlast
    -- Lenses for Ghost
    , gname
    , gdir
    , gpos
    , gstrt
    , gstate
    , gtlast
    , gpathback
    -- Lenses for Items
    , pellets
    , ppellets
    , gstscore
    , fruits
    -- Lenses for Fruit
    , fname
    , fduration
    , fdelay
    , fpos
    , fpoints
    ) where

import qualified Data.Matrix as M
import qualified Data.Text   as Txt
import System.Random                ( StdGen )
import Lens.Micro.TH                ( makeLenses )

data TimeEvent = Tick Int deriving ( Show )

data Direction = North | South | East | West deriving ( Show, Eq )

data Status = Running
            | PwrRunning Int
            | GameOver
            | LevelOver
            | ReplayLvl
            deriving ( Show, Eq )

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
          | Pinky
          | Blinky
          | Inky
          | Clyde
          | BlueGhost
          | WhiteGhost
          | GhostEyes
          -- Fruit
          | Cherry
          | Strawberry
          | Orange
          | Apple
          | Melon
          -- Maze walls
          | Wall Txt.Text
          | OneWay Direction
          -- Warp tiles
          | Warp Direction Point
          deriving (Show, Eq)

type Maze = M.Matrix Tile

type Point = (Int, Int)

type GameSt = Either String Game

data GhostState = Normal | Edible | EyesOnly deriving (Show, Eq)

data Ghost = Ghost { _gname     :: Tile               -- Name/tile for ghost
                   , _gdir      :: Direction          -- Current direction
                   , _gpos      :: Point              -- Current position
                   , _gstrt     :: (Point, Direction) -- Initial pos. & dir.
                   , _gstate    :: GhostState         -- Ghost state
                   , _gtlast    :: Int                -- Time of last move
                   , _gpathback :: [Point]            -- Path back to start
                   } deriving ( Show )

instance Eq Ghost where
    (==) g1 g2 = _gname g1 == _gname g2

data Fruit = Fruit { _fname     :: Tile         -- Name/tile for fruit
                   , _fduration :: Int          -- How long fruit lasts
                   , _fdelay    :: Int          -- Time before it appears
                   , _fpos      :: Point        -- Where the fruit appears
                   , _fpoints   :: Int          -- Point value of the fruit
                   } deriving ( Show )

instance Eq Fruit where
    (==) f1 f2 = _fname f1 == _fname f2

data PacMan = PacMan { _pdir   :: Direction          -- Current direction
                     , _ppos   :: Point              -- Current position
                     , _pstrt  :: (Point, Direction) -- Initial pos. & dir.
                     , _ptlast :: Int                -- Time of last move
                     } deriving ( Show )

data Items = Items { _pellets  :: Int
                   , _ppellets :: Int
                   , _gstscore :: Int
                   , _fruits   :: [(Tile, Int)]
                   } deriving ( Show )

data Game = Game { _maze     :: Maze        -- Level maze
                 , _items    :: Items       -- Summary of items and score
                 , _pacman   :: PacMan      -- Player
                 , _ghosts   :: [ Ghost ]   -- List of ghosts
                 , _fruit    :: Maybe Fruit -- Fruit for the level
                 , _rgen     :: StdGen      -- Standard generator
                 , _npellets :: Int         -- Pellets remaining in level
                 , _oneups   :: Int         -- Oneups remaining
                 , _status   :: Status      -- Game status
                 , _level    :: Int         -- Current level number
                 , _pwrtime  :: Int         -- Duration of power pellets
                 , _time     :: Int         -- Current in-game time
                 , _dtime    :: Int         -- Time since last update
                 , _pwrmult  :: Int         -- Score multiplier for ghost
                 , _msg      :: Maybe (String, Int) -- In-game message
                 } deriving ( Show )

makeLenses ''Game
makeLenses ''Items
makeLenses ''PacMan
makeLenses ''Ghost
makeLenses ''Fruit
