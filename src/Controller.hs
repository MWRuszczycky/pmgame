module Controller
    ( eventRouter
    ) where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import qualified Types        as T
import Lens.Micro                   ( (&), (^.), (.~), (%~) )
import System.Random                ( StdGen
                                    , randomR               )
import Data.List                    ( delete
                                    , nub                   )
import Brick.Types                  ( BrickEvent (..)
                                    , Next
                                    , EventM                )
import Types                        ( Game (..)
                                    , Maze (..)
                                    , Ghost (..)
                                    , PacMan (..)
                                    , Point (..)
                                    , Tile (..)
                                    , TimeEvent (..)
                                    , Direction (..)
                                    , Status (..)           )
import Maze                         ( sumPair
                                    , isFree
                                    , dirToPair             )
import Brick.Main                   ( continue
                                    , halt                  )

---------------------------------------------------------------------
-- Event routers

eventRouter :: Game -> BrickEvent () TimeEvent -> EventM () ( Next Game )
eventRouter g e = case g ^. T.status of
                       Running   -> routeRunning g e
                       GameOver  -> routeGameOver g e
                       LevelOver -> routeLevelOver g e

routeRunning :: Game -> BrickEvent () TimeEvent -> EventM () ( Next Game )
routeRunning g (VtyEvent (V.EvKey V.KEsc [] )) = halt g
routeRunning g (VtyEvent (V.EvKey k ms ))      = continue . keyEvent k ms $ g
routeRunning g (VtyEvent (V.EvResize _ _ ))    = continue g
routeRunning g (AppEvent Tick)                 = continue . tickEvent $ g
routeRunning g _                               = continue g

routeGameOver :: Game -> BrickEvent () TimeEvent -> EventM () ( Next Game )
routeGameOver g (VtyEvent (V.EvKey V.KEsc [] ))   = halt g
routeGameOver g (VtyEvent (V.EvKey V.KEnter [] )) = halt g
routeGameOver g (VtyEvent (V.EvKey _ _ ))         = continue g
routeGameOver g (VtyEvent (V.EvResize _ _ ))      = continue g
routeGameOver g _                                 = continue g

routeLevelOver :: Game -> BrickEvent () TimeEvent -> EventM () ( Next Game )
routeLevelOver g (VtyEvent (V.EvKey V.KEsc [] ))   = halt g
routeLevelOver g (VtyEvent (V.EvKey V.KEnter [] )) = halt g
routeLevelOver g (VtyEvent (V.EvKey _ _ ))         = continue g
routeLevelOver g (VtyEvent (V.EvResize _ _ ))      = continue g
routeLevelOver g _                                 = continue g

---------------------------------------------------------------------
-- Event handlers for running game

tickEvent :: Game -> Game
tickEvent g = updateStatus g . moveGhosts . movePlayer $ g

keyEvent :: V.Key -> [V.Modifier] -> Game -> Game
keyEvent V.KLeft  ms g = g & T.pacman . T.pdir .~ West
keyEvent V.KRight ms g = g & T.pacman . T.pdir .~ East
keyEvent V.KUp    ms g = g & T.pacman . T.pdir .~ North
keyEvent V.KDown  ms g = g & T.pacman . T.pdir .~ South
keyEvent _        _  g = g

---------------------------------------------------------------------
-- Event handlers for game over

---------------------------------------------------------------------
-- Game state management

wasCaptured :: Game -> Game -> Bool
wasCaptured g0 g1 = any ( pathsCrossed (p0, p1) ) ( zip gs0 gs1 )
    where p0 = g0 ^. T.pacman . T.ppos
          p1 = g1 ^. T.pacman . T.ppos
          gs0 = map ( ^. T.gpos ) ( g0 ^. T.ghosts )
          gs1 = map ( ^. T.gpos ) ( g1 ^. T.ghosts )

pathsCrossed :: (Point, Point) -> (Point, Point) -> Bool
pathsCrossed (p0, p1) (g0, g1) = p1 == g1 || p1 == g0 && p0 == g1

updateStatus :: Game -> Game -> Game
updateStatus g0 g1
    | allPellets        = g1 & T.status .~ LevelOver
    | wasCaptured g0 g1 = g1 & T.status .~ GameOver
    | otherwise         = g1 & T.status .~ Running
    where allPellets = g1 ^. T.remaining == 0

---------------------------------------------------------------------
-- Player updating

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

---------------------------------------------------------------------
-- Ghost updating

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

randomDirections :: StdGen -> [Direction] -> (StdGen, [Direction])
randomDirections r0 [] = (r0, [])
randomDirections r0 ds0 = (r, d:ds)
    where (k,r1)  = randomR (0, length ds0 - 1) r0
          d       = ds0 !! k
          ds1     = delete d . nub $ ds0
          (r,ds)  = randomDirections r1 ds1
