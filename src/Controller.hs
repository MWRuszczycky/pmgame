module Controller
    ( eventRouter
    ) where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import System.Random                ( StdGen            )
import Data.List                    ( foldl', delete    )
import Brick.Types                  ( BrickEvent (..)
                                    , Next
                                    , EventM            )
import Types                        ( Game (..)
                                    , Maze (..)
                                    , Ghost (..)
                                    , Tile (..)
                                    , TimeEvent (..)
                                    , Direction (..)    )
import Maze                         ( sumPair
                                    , isFree
                                    , findTile
                                    , dirToPair
                                    , randomDirections  )
import Brick.Main                   ( continue
                                    , halt              )

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
keyEvent V.KLeft  ms s = s { pdir = West  }
keyEvent V.KRight ms s = s { pdir = East  }
keyEvent V.KUp    ms s = s { pdir = North }
keyEvent V.KDown  ms s = s { pdir = South }
keyEvent _        _  s = s

moveGhosts :: Game -> Game
moveGhosts s = s { maze = m, ghosts = gs, rgen = r }
    where (m, gs, r) = foldl' moveGhost (maze s, [], rgen s) . ghosts $ s

moveGhost :: (Maze, [Ghost], StdGen) -> Ghost -> (Maze, [Ghost], StdGen)
moveGhost (m0, gs, r0) (Ghost nm d0) = (m1, (Ghost nm d1):gs, r1)
    where Just p0  = findTile nm m0
          dirs     = [North, South, East, West] ++ replicate 10 d0
          (r1, ds) = randomDirections r0 dirs
          ps       = [ (d, sumPair p0 . dirToPair $ d) | d <- ds ]
          (d1, p1) = head . dropWhile (not . isFree m0 . snd) $ ps
          old      = uncurry M.getElem p0 m0
          nxt      = uncurry M.getElem p1 m0
          m1       = M.setElem (delete nm old) p0 . M.setElem (nm:nxt) p1 $ m0

movePlayer :: Game -> Game
movePlayer s
    | isFree m0 p1 = s { maze = m1, score = scr1 }
    | otherwise    = s
    where d       = dirToPair . pdir $ s
          m0      = maze s
          Just p0 = findTile Player m0
          p1      = sumPair p0 d
          m1      = M.setElem [] p0 . M.setElem [Player] p1 $ m0
          scr1    = case uncurry M.getElem p1 m0 of
                         [Pellet]  -> (+10) . score $ s
                         otherwise -> score s
