module Types
    ( Maze
    , Tile (..)
    , St (..)
    , Direction (..)
    , TimeEvent (..)
    ) where

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

type Maze = [[Tile]]

data St = St { maze      :: Maze
             , score     :: Int
             , direction :: Direction }
