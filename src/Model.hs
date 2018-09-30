module Model
    ( movePlayer
    , moveGhosts
    , tileGhosts
    , isGhost
    , isPlayer
    , isWall
    , isPellet
    , restartLevel
    , getNxtLevel
    , updateGame
    , updateGamePwr
    ) where

import qualified Data.Matrix as M
import qualified Types       as T
import Lens.Micro                   ( (&), (^.), (.~), (%~), set    )
import Data.Matrix                  ( (!) )
import Data.List                    ( delete
                                    , nub                           )
import System.Random                ( StdGen
                                    , randomR                       )
import Types                        ( Tile      (..)
                                    , Game      (..)
                                    , Status    (..)
                                    , Point     (..)
                                    , Maze      (..)
                                    , Direction (..)
                                    , PacMan    (..)
                                    , Ghost     (..)                )

---------------------------------------------------------------------
-- Pure functions for managing game state

-- Tile subtypes

isWall :: Tile -> Bool
-- ^Evaluate whether a tile is a wall tile.
isWall t = elem t ws
    where ws = [ HBar, VBar, LTee, UTee, RTee, DTee, RDCr, LDCr, RUCr, LUCr ]

isGhost :: Tile -> Bool
-- ^Evaluate whether a tile is a ghost.
isGhost t = elem t [ Blinky, Pinky, Inky, Clyde, BlueGhost, WhiteGhost ]

isPellet :: Tile -> Bool
-- ^Evaluate whether a tile is a pellet.
isPellet t = elem t [ PwrPellet, Pellet ]

isPlayer :: Tile -> Bool
-- ^Evaluate whether a tile is the player.
isPlayer t = t == Player

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

updateGame :: Game -> Game -> Game
-- ^Determine status of a running game at time t.
updateGame g0 g1
    | allPellets        = g1 & T.status .~ LevelOver
    | wasCaptured g0 g1 = handleCapture g0 g1
    | wasPowered g0 g1  = powerGame g1
    | otherwise         = g1
    where allPellets = g1 ^. T.npellets == 0

updateGamePwr :: Game -> Game -> Game
-- ^Determine status of a running game at time t after eating a
-- power pellet.
updateGamePwr g0 g1
    | allPellets       = g1 & T.status .~ LevelOver
    | wasPowered g0 g1 = powerGame g1
    | pwrLeft g1 <= 0  = depowerGame g1
    | otherwise        = g1
    -- | wasCaptured g0 g1 = handleCapture g0 g1
    where allPellets = g1 ^. T.npellets == 0

-- Unexported

powerGame :: Game -> Game
powerGame g = g & T.ghosts .~ ghsts & T.status .~ PwrRunning ( g ^. T.time )
    where ghsts = map ( set T.gedible True ) $ g ^. T.ghosts

depowerGame :: Game -> Game
depowerGame g = g & T.ghosts .~ ghsts & T.status .~ Running
    where ghsts = map ( set T.gedible False ) $ g ^. T.ghosts

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
                     PwrPellet -> (M.setElem Empty p1 m0, 1)
                     otherwise -> (m0, 0)

---------------------------------------------------------------------
-- Ghost updating

-- Exported

moveGhosts :: Game -> Game
moveGhosts g = g & T.maze .~ m & T.ghosts .~ gsts & T.rgen .~ r
    where (m, gsts, r) = foldr moveGhost start $ g ^. T.ghosts
          start = (g ^. T.maze, [], g ^. T.rgen )

tileGhosts :: Game -> [(Point, Tile)]
tileGhosts g = [ (gst ^. T.gpos, tileGhost g gst) | gst <- g ^. T.ghosts ]

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

tileGhost :: Game -> Ghost -> Tile
tileGhost g gst
    | isBlueGhost  = BlueGhost
    | isWhiteGhost = WhiteGhost
    | otherwise    = gst ^. T.gname
    where isBlueGhost  = gst ^. T.gedible && isBlue (pwrLeft g) g
          isWhiteGhost = gst ^. T.gedible

isBlue :: Int -> Game -> Bool
isBlue tlft g
    | tlft < half = True
    | otherwise   = even . quot tlft $ ( g ^. T.dtime )
    where half = quot ( g ^. T.pwrtime ) 2

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

wasPowered :: Game -> Game -> Bool
wasPowered g0 g1 = t1 == PwrPellet
    where t1 = (g0 ^. T.maze) ! (g1 ^. T.pacman . T.ppos)

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

pwrLeft :: Game -> Int
-- ^Evaluate how much power time is left after eating a power pellet.
pwrLeft g
    | pt - dt > 0 = dt
    | otherwise   = 0
    where pt = g ^. T.pwrtime
          dt = case g ^. T.status of
                    PwrRunning t -> g ^. T.time - t
                    otherwise    -> 0
