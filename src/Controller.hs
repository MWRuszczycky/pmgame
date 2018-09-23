module Controller
    ( eventRouter
    ) where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import qualified Types        as T
import Lens.Micro                   ( (&), (^.), (.~), (%~) )
import System.Random                ( StdGen            )
import Data.List                    ( foldl', delete    )
import Brick.Types                  ( BrickEvent (..)
                                    , Next
                                    , EventM            )
import Types                        ( Game (..)
                                    , Maze (..)
                                    , Ghost (..)
                                    , PacMan (..)
                                    , Tile (..)
                                    , TimeEvent (..)
                                    , Direction (..)    )
import Maze                         ( sumPair
                                    , isFree
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
tickEvent = chkDone . moveGhosts . movePlayer

keyEvent :: V.Key -> [V.Modifier] -> Game -> Game
keyEvent V.KLeft  ms g = g & T.pacman . T.pdir .~ West
keyEvent V.KRight ms g = g & T.pacman . T.pdir .~ East
keyEvent V.KUp    ms g = g & T.pacman . T.pdir .~ North
keyEvent V.KDown  ms g = g & T.pacman . T.pdir .~ South
keyEvent _        _  g = g

chkDone :: Game -> Game
chkDone = id

moveGhosts :: Game -> Game
moveGhosts g = g & T.maze .~ m & T.ghosts .~ gsts & T.rgen .~ r
    where (m, gsts, r) = foldl' moveGhost start $ g ^. T.ghosts
          start = (g ^. T.maze, [], g ^. T.rgen )

moveGhost :: (Maze, [Ghost], StdGen) -> Ghost -> (Maze, [Ghost], StdGen)
moveGhost (m, gsts, r0) (Ghost nm d0 p0) = (m, (Ghost nm d1 p1):gsts, r1)
    where dirs     = [North, South, East, West] ++ replicate 10 d0
          (r1, ds) = randomDirections r0 dirs
          ps       = [ (d, sumPair p0 . dirToPair $ d) | d <- ds ]
          (d1, p1) = head . dropWhile (not . isFree m . snd) $ ps

movePlayer :: Game -> Game
movePlayer g
    | isFree m0 p1 = g & T.maze .~ m1
                       & T.items . T.pellets %~ (+ s1)
                       & T.remaining %~ (subtract s1)
                       & T.pacman . T.ppos .~ p1
    | otherwise    = g
    where PacMan d p0 = g ^. T.pacman
          m0 = g ^. T.maze
          p1 = sumPair p0 . dirToPair $ d
          m1 = M.setElem Empty p0 m0
          s1 = case uncurry M.getElem p1 m0 of
                    Pellet    -> 1
                    otherwise -> 0
