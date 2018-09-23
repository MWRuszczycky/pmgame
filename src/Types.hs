module Types
    ( Tile (..)
    , Maze (..)
    , Game (..)
    , Ghost (..)
    , Direction (..)
    , TimeEvent (..)
    ) where

import qualified Data.Matrix as M

data TimeEvent = Tick deriving ( Show )

data Direction = North | South | East | West deriving ( Show, Eq )

data Tile = Player
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

type Maze = M.Matrix [ Tile ]

data Ghost = Ghost { name :: Tile
                   , gdir :: Direction
                   } deriving ( Show )

data Game = Game { maze   :: Maze
                 , score  :: Int
                 , pdir   :: Direction
                 , ghosts :: [ Ghost ]
                 } deriving ( Show )
