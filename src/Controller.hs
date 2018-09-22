module Controller
    ( eventRouter
    ) where

import qualified Graphics.Vty as V
import Brick.Types  ( BrickEvent (..), Next, EventM )
import Types        ( St (..), Tile (..), TimeEvent (..), Direction (..) )
import Maze         ( sumPair, isFree, update, findTile, query, dirToPair )
import Brick.Main   ( continue, halt )

---------------------------------------------------------------------
-- Interface

eventRouter :: St -> BrickEvent () TimeEvent -> EventM () ( Next St )
eventRouter s (VtyEvent (V.EvKey V.KEsc [] )) = halt s
eventRouter s (VtyEvent (V.EvKey k ms ))      = continue . keyEvent k ms $ s
eventRouter s (VtyEvent (V.EvResize _ _))     = continue s
eventRouter s (AppEvent Tick)                 = continue . tickEvent $ s
eventRouter s _                               = continue s

---------------------------------------------------------------------
-- Helpers

tickEvent :: St -> St
tickEvent = moveGhosts . movePlayer

keyEvent :: V.Key -> [V.Modifier] -> St -> St
keyEvent V.KLeft  ms s = s { direction = West  }
keyEvent V.KRight ms s = s { direction = East  }
keyEvent V.KUp    ms s = s { direction = North }
keyEvent V.KDown  ms s = s { direction = South }
keyEvent _        _  s = s

moveGhosts :: St -> St
moveGhosts = id

movePlayer :: St -> St
movePlayer s
    | isFree p1 m0 = s { maze = m1, score = scr1 }
    | otherwise    = s
    where d       = dirToPair . direction $ s
          m0      = maze s
          Just p0 = findTile Player m0
          p1      = sumPair p0 d
          m1      = update p0 Empty . update p1 Player $ m0
          scr1    = case query p1 m0 of
                        Pellet    -> (+10) . score $ s
                        otherwise -> score s
