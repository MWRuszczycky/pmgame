{-# LANGUAGE TemplateHaskell #-}
module Types
    ( Tile (..)
    , Maze (..)
    , Game (..)
    , Ghost (..)
    , PacMan (..)
    , Direction (..)
    , TimeEvent (..)
    -- Lenses for Game
    , maze
    , score
    , pacman
    , ghosts
    , rgen
    -- Lenses for PacMan
    , pdir
    , ppos
    -- Lenses for Ghost
    , gname
    , gdir
    , gpos
    ) where

import qualified Data.Matrix as M
import System.Random                ( StdGen )
import Lens.Micro.TH                ( makeLenses )

data TimeEvent = Tick deriving ( Show )

data Direction = North | South | East | West deriving ( Show, Eq )

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
$(makeLenses ''Ghost)

data PacMan = PacMan { _pdir :: Direction
                     , _ppos :: (Int, Int)
                     } deriving ( Show )
$(makeLenses ''PacMan)

data Game = Game { _maze   :: Maze
                 , _score  :: Int
                 , _pacman :: PacMan
                 , _ghosts :: [ Ghost ]
                 , _rgen   :: StdGen
                 } deriving ( Show )
$(makeLenses ''Game)
