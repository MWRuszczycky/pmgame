module Controller
    ( eventRouter
    ) where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import Data.List    ( foldl', (\\) )
import Brick.Types  ( BrickEvent (..), Next, EventM )
import Types        ( Game (..), Tile (..), TimeEvent (..), Direction (..) )
import Maze         ( sumPair, isFree, findTile, dirToPair )
import Brick.Main   ( continue, halt )

---------------------------------------------------------------------
-- Interface

eventRouter :: Game -> BrickEvent () TimeEvent -> EventM () ( Next Game )
eventRouter s (VtyEvent (V.EvKey V.KEsc [] )) = halt s
eventRouter s (VtyEvent (V.EvKey k ms ))      = continue . keyEvent k ms $ s
eventRouter s (VtyEvent (V.EvResize _ _))     = continue s
eventRouter s (AppEvent Tick)                 = continue . tickEvent $ s
eventRouter s _                               = continue s

---------------------------------------------------------------------
-- Helpers

tickEvent :: Game -> Game
tickEvent = moveGhosts . movePlayer

keyEvent :: V.Key -> [V.Modifier] -> Game -> Game
keyEvent V.KLeft  ms s = s { direction = West  }
keyEvent V.KRight ms s = s { direction = East  }
keyEvent V.KUp    ms s = s { direction = North }
keyEvent V.KDown  ms s = s { direction = South }
keyEvent _        _  s = s

moveGhosts :: Game -> Game
moveGhosts s = s { maze = m, ghosts = gs }
    where (m, gs) = foldl' moveGhost (maze s, []) . ghosts $ s

moveGhost :: (M.Matrix Tile, [(Tile, Direction)]) -> (Tile, Direction)
             -> (M.Matrix Tile, [(Tile, Direction)])
moveGhost (m0, gs) (t,d0) = (m1, (t,d1):gs)
    where Just p0  = findTile t m0
          dirs     = d0 : ( [North, East, South, West] \\ [d0] )
          ps       = [ (d, sumPair p0 . dirToPair $ d) | d <- dirs ]
          (d1, p1) = head . dropWhile (not . isFree m0 . snd) $ ps
          m1       = M.setElem Empty p0 . M.setElem t p1 $ m0

movePlayer :: Game -> Game
movePlayer s
    | isFree m0 p1 = s { maze = m1, score = scr1 }
    | otherwise    = s
    where d       = dirToPair . direction $ s
          m0      = maze s
          Just p0 = findTile Player m0
          p1      = sumPair p0 d
          m1      = M.setElem Empty p0 . M.setElem Player p1 $ m0
          scr1    = case uncurry M.getElem p1 m0 of
                         Pellet    -> (+10) . score $ s
                         otherwise -> score s
