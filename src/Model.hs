module Model
    ( movePlayer
    , moveGhosts
    , restartLevel
    , getNxtLevel
    , updateStatus
    ) where

import qualified Data.Matrix as M
import qualified Types       as T
import Lens.Micro                   ( (&), (^.), (.~), (%~) )
import Data.Matrix                  ( (!) )
import Data.List                    ( delete
                                    , nub                   )
import System.Random                ( StdGen
                                    , randomR               )
import Types                        ( Tile      (..)
                                    , Game      (..)
                                    , Status    (..)
                                    , Point     (..)
                                    , Maze      (..)
                                    , Direction (..)
                                    , PacMan    (..)
                                    , Ghost     (..)           )

---------------------------------------------------------------------
-- Pure functions for managing game state

---------------------------------------------------------------------
-- Game and level restarting

-- Exported

restartLevel :: Game -> Game
-- ^Restarts the current level.
restartLevel g = g & T.status .~ Running
                   & T.pacman %~ resetPacMan
                   & T.ghosts %~ map resetGhost
                   & T.oneups %~ subtract 1

-- Unexported

resetPacMan :: PacMan -> PacMan
-- ^Resets player to origin position and direction.
resetPacMan p = p & T.ppos .~ pos0 & T.pdir .~ dir0
    where (pos0, dir0) = p ^. T.pstrt

resetGhost :: Ghost -> Ghost
-- ^Reset a ghost to its original position and direction.
resetGhost g = g & T.gpos .~ pos0 & T.gdir .~ dir0
    where (pos0, dir0) = g ^. T.gstrt

getNxtLevel :: Game -> Game -> Game
getNxtLevel g0 g1 = g1 & T.items  .~ ( g0 ^. T.items )
                       & T.level  .~ ( succ $ g0 ^. T.level )
                       & T.oneups .~ ( g0 ^. T.oneups )

---------------------------------------------------------------------
-- Game status management

-- Exported

updateStatus :: Game -> Game -> Game
-- ^Determine status of a running game.
updateStatus g0 g1
    | allPellets        = g1 & T.status .~ LevelOver
    | wasCaptured g0 g1 = handleCapture g0 g1
    | otherwise         = g1 & T.status .~ Running
    where allPellets = g1 ^. T.npellets == 0

-- Unexported

handleCapture :: Game -> Game -> Game
-- ^Update game state following player capture by a ghost.
handleCapture g0 g1
    | g0 ^. T.oneups == 0 = g1 & T.status .~ GameOver
    | otherwise           = g1 & T.status .~ ReplayLvl

---------------------------------------------------------------------
-- Player updating

-- Exported

movePlayer :: Game -> Game
movePlayer g
    | isFree m0 p1 = g & T.maze .~ m1
                       & T.items . T.pellets %~ (+ ds)
                       & T.npellets %~ (subtract ds)
                       & T.pacman . T.ppos .~ p1
    | otherwise    = g
    where p0  = g ^. T.pacman . T.ppos
          dir = g ^. T.pacman . T.pdir
          m0  = g ^. T.maze
          p1  = getNxtPos p0 (m0 ! p0) dir
          (m1, ds) = case (m0 ! p1) of
                     Pellet    -> (M.setElem Empty p1 m0, 1)
                     otherwise -> (m0, 0)

---------------------------------------------------------------------
-- Ghost updating

-- Exported

moveGhosts :: Game -> Game
moveGhosts g = g & T.maze .~ m & T.ghosts .~ gsts & T.rgen .~ r
    where (m, gsts, r) = foldr moveGhost start $ g ^. T.ghosts
          start = (g ^. T.maze, [], g ^. T.rgen )

-- Unexported

moveGhost :: Ghost -> (Maze, [Ghost], StdGen) -> (Maze, [Ghost], StdGen)
moveGhost gst0 (m, gsts, r0) = (m, gst1:gsts, r1)
    where p0       = gst0 ^. T.gpos
          dir      = gst0 ^. T.gdir
          dirs     = [North, South, East, West] ++ replicate 20 dir
          (r1, ds) = randomDirections r0 dirs
          ps       = [ (d, getNxtPos p0 (m ! p0) d) | d <- ds ]
          (d1, p1) = head . dropWhile (not . isFree m . snd) $ ps
          gst1     = gst0 & T.gpos .~ p1 & T.gdir .~ d1

randomDirections :: StdGen -> [Direction] -> (StdGen, [Direction])
randomDirections r0 [] = (r0, [])
randomDirections r0 ds0 = (r, d:ds)
    where (k,r1)  = randomR (0, length ds0 - 1) r0
          d       = ds0 !! k
          ds1     = delete d . nub $ ds0
          (r,ds)  = randomDirections r1 ds1

---------------------------------------------------------------------
-- Utilities for time-dependent changes in the game state

-- Unexported

wasCaptured :: Game -> Game -> Bool
-- ^Determine whether player was captured by a ghost in last move.
wasCaptured g0 g1 = any ( isCapture (p0, p1) ) ( zip gs0 gs1 )
    where p0 = g0 ^. T.pacman . T.ppos
          p1 = g1 ^. T.pacman . T.ppos
          gs0 = map ( ^. T.gpos ) ( g0 ^. T.ghosts )
          gs1 = map ( ^. T.gpos ) ( g1 ^. T.ghosts )

isCapture :: (Point, Point) -> (Point, Point) -> Bool
-- ^Determine whether player and ghost are either occupying the same
-- tile or have crossed paths in resulting in a capture.
isCapture (p0, p1) (g0, g1) = p1 == g1 || p1 == g0 && p0 == g1

dirToShift :: Direction -> Point
-- ^Maps directions to single-tick displacements.
dirToShift West  = (0,-1)
dirToShift East  = (0, 1)
dirToShift North = (-1,0)
dirToShift South = (1, 0)

getNxtPos :: Point -> Tile -> Direction -> Point
-- ^Get next maze position based on current position, tile and
-- direction of movement.
getNxtPos p0 (Warp wd p1) d
    | d == wd   = p1
    | otherwise = moveOneCell p0 d
getNxtPos p0 _ d = moveOneCell p0 d

moveOneCell :: Point -> Direction -> Point
moveOneCell p = go p . dirToShift
    where go (x0,y0) (x1,y1) = (x0 + x1, y0 + y1)

isFree :: Maze -> Point -> Bool
-- ^Evaluate whether a point in the maze is free to be entered.
isFree m (r,c) = case M.safeGet r c m of
                      Nothing -> False
                      Just t  -> not . isWall $ t

isWall :: Tile -> Bool
-- ^Evaluate whether a tile is a wall tile.
isWall t = elem t ws
    where ws = [ HBar, VBar, LTee, UTee, RTee, DTee, RDCr, LDCr, RUCr, LUCr ]
