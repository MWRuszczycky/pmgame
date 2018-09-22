module Types
    ( Tile (..)
    , St (..)
    , Direction (..)
    , TimeEvent (..)
    ) where

import qualified Data.Matrix as M

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

data St = St { maze      :: M.Matrix Tile
             , score     :: Int
             , direction :: Direction }
