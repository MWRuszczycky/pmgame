{-# LANGUAGE TemplateHaskell #-}
module Types
    ( Tile (..)
    , Maze (..)
    , Game (..)
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
    , remaining
    -- Lenses for PacMan
    , pdir
    , ppos
    -- Lenses for Ghost
    , gname
    , gdir
    , gpos
    -- Lenses for Items
    , pellets
    ) where

import qualified Data.Matrix as M
import System.Random                ( StdGen )
import Lens.Micro.TH                ( makeLenses )

data TimeEvent = Tick deriving ( Show )

data Direction = North | South | East | West deriving ( Show, Eq )

data Status = Running | GameOver | LevelFinished deriving ( Show, Eq )

data Tile = Player
          | Empty
          | Pellet
          | Pinky
          | Blinky
          | Inky
          | Clyde
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
          deriving (Show, Eq)

type Maze = M.Matrix Tile

data Ghost = Ghost { _gname :: Tile
                   , _gdir  :: Direction
                   , _gpos  :: (Int, Int)
                   } deriving ( Show )

data PacMan = PacMan { _pdir :: Direction
                     , _ppos :: (Int, Int)
                     } deriving ( Show )

data Items = Items { _pellets :: Int
                   } deriving ( Show )

data Game = Game { _maze      :: Maze
                 , _items     :: Items
                 , _pacman    :: PacMan
                 , _ghosts    :: [ Ghost ]
                 , _rgen      :: StdGen
                 , _remaining :: Int
                 } deriving ( Show )

makeLenses ''Game
makeLenses ''Items
makeLenses ''PacMan
makeLenses ''Ghost
