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
                                    , Direction (..)
                                    , Status (..)       )
import Maze                         ( sumPair
                                    , isFree
                                    , dirToPair
                                    , wasCaptured
                                    , chkStatus
                                    , randomDirections  )
import Brick.Main                   ( continue
                                    , halt              )

---------------------------------------------------------------------
-- Interface

eventRouter :: Game -> BrickEvent () TimeEvent -> EventM () ( Next Game )
eventRouter g e = case chkStatus g of
                       Running   -> routeRunning g e
                       GameOver  -> routeGameOver g e
                       LevelOver -> routeLevelOver g e

routeRunning :: Game -> BrickEvent () TimeEvent -> EventM () ( Next Game )
routeRunning g (VtyEvent (V.EvKey V.KEsc [] )) = halt g
routeRunning g (VtyEvent (V.EvKey k ms ))      = continue . keyEvent k ms $ g
routeRunning g (VtyEvent (V.EvResize _ _))     = continue g
routeRunning g (AppEvent Tick)                 = continue . tickEvent $ g
routeRunning g _                               = continue g

routeGameOver :: Game -> BrickEvent () TimeEvent -> EventM () ( Next Game )
routeGameOver g e = halt g

routeLevelOver :: Game -> BrickEvent () TimeEvent -> EventM () ( Next Game )
routeLevelOver g e = halt g

---------------------------------------------------------------------
-- Helpers

tickEvent :: Game -> Game
tickEvent g0 = g1 & T.captured .~ (wasCaptured g0 g1)
    where g1 = moveGhosts . movePlayer $ g0

keyEvent :: V.Key -> [V.Modifier] -> Game -> Game
keyEvent V.KLeft  ms g = g & T.pacman . T.pdir .~ West
keyEvent V.KRight ms g = g & T.pacman . T.pdir .~ East
keyEvent V.KUp    ms g = g & T.pacman . T.pdir .~ North
keyEvent V.KDown  ms g = g & T.pacman . T.pdir .~ South
keyEvent _        _  g = g

moveGhosts :: Game -> Game
moveGhosts g = g & T.maze .~ m & T.ghosts .~ gsts & T.rgen .~ r
    where (m, gsts, r) = foldr moveGhost start $ g ^. T.ghosts
          start = (g ^. T.maze, [], g ^. T.rgen )

moveGhost :: Ghost -> (Maze, [Ghost], StdGen) -> (Maze, [Ghost], StdGen)
moveGhost (Ghost nm d0 p0) (m, gsts, r0) = (m, (Ghost nm d1 p1):gsts, r1)
    where dirs     = [North, South, East, West] ++ replicate 20 d0
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
