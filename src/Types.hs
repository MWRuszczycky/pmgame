{-# LANGUAGE TemplateHaskell #-}
module Types
    ( Tile (..)
    , Maze (..)
    , Point (..)
    , Game (..)
    , GameSt (..)
    , Ghost (..)
    , PacMan (..)
    , Direction (..)
    , TimeEvent (..)
    , Items (..)
    , Status (..)
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
    , pwrmult
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
    , gedible
    , gtlast
    -- Lenses for Items
    , pellets
    , ppellets
    , gstscore
    ) where

import qualified Data.Matrix as M
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

data Tile = Player
          | Empty
          | Pellet
          | PwrPellet
          | Pinky
          | Blinky
          | Inky
          | Clyde
          | BlueGhost
          | WhiteGhost
          | HBar
          | VBar
          | Cros
          | LTee
          | RTee
          | DTee
          | UTee
          | LUCr
          | RUCr
          | LDCr
          | RDCr
          | Warp Direction Point
          deriving (Show, Eq)

type Maze = M.Matrix Tile

type Point = (Int, Int)

type GameSt = Either String Game

data Ghost = Ghost { _gname   :: Tile               -- Name/tile for ghost
                   , _gdir    :: Direction          -- Current direction
                   , _gpos    :: Point              -- Current position
                   , _gstrt   :: (Point, Direction) -- Initial pos. & dir.
                   , _gedible :: Bool               -- Ghost can be eaten
                   , _gtlast  :: Int                -- Time of last move
                   } deriving ( Show )

instance Eq Ghost where
    (==) g1 g2 = _gname g1 == _gname g2

data PacMan = PacMan { _pdir   :: Direction          -- Current direction
                     , _ppos   :: Point              -- Current position
                     , _pstrt  :: (Point, Direction) -- Initial pos. & dir.
                     , _ptlast :: Int                -- Time of last move
                     } deriving ( Show )

data Items = Items { _pellets  :: Int
                   , _ppellets :: Int
                   , _gstscore :: Int
                   } deriving ( Show )

data Game = Game { _maze     :: Maze
                 , _items    :: Items
                 , _pacman   :: PacMan
                 , _ghosts   :: [ Ghost ]
                 , _rgen     :: StdGen
                 , _npellets :: Int
                 , _oneups   :: Int
                 , _status   :: Status
                 , _level    :: Int
                 , _pwrtime  :: Int
                 , _time     :: Int
                 , _dtime    :: Int
                 , _pwrmult  :: Int
                 } deriving ( Show )

makeLenses ''Game
makeLenses ''Items
makeLenses ''PacMan
makeLenses ''Ghost
