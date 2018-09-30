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
    -- Lenses for PacMan
    , pdir
    , ppos
    , pstrt
    -- Lenses for Ghost
    , gname
    , gdir
    , gpos
    , gstrt
    , gedible
    -- Lenses for Items
    , pellets
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

data Ghost = Ghost { _gname   :: Tile
                   , _gdir    :: Direction
                   , _gpos    :: Point
                   , _gstrt   :: (Point, Direction)
                   , _gedible :: Bool
                   } deriving ( Show )

data PacMan = PacMan { _pdir  :: Direction
                     , _ppos  :: Point
                     , _pstrt :: (Point, Direction)
                     } deriving ( Show )

data Items = Items { _pellets :: Int
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
                 } deriving ( Show )

makeLenses ''Game
makeLenses ''Items
makeLenses ''PacMan
makeLenses ''Ghost
