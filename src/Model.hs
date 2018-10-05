module Model
    ( isGhost
    , isPellet
    , isPlayer
    , isWall
    , getNxtLevel
    , restartLevel
    , updateGame
    , movePlayer
    , moveGhosts
    , tileGhosts
    ) where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Types       as T
import Lens.Micro                   ( (&), (^.), (.~), (%~), set    )
import Data.Matrix                  ( (!)                           )
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
---------------------------------------------------------------------
-- Pure functions for managing game state

-- =============================================================== --
-- Tile subtypes

-- Exported

isGhost :: Tile -> Bool
-- ^Evaluate whether a tile is a ghost.
isGhost t = elem t [ Blinky, Pinky, Inky, Clyde, BlueGhost, WhiteGhost ]

isWall :: Tile -> Bool
-- ^Evaluate whether a tile is a wall tile.
isWall t = elem t ws
    where ws = [ HBar, VBar, LTee, UTee, RTee, DTee, RDCr, LDCr, RUCr, LUCr ]

isPellet :: Tile -> Bool
-- ^Evaluate whether a tile is a pellet.
isPellet t = elem t [ PwrPellet, Pellet ]

isPlayer :: Tile -> Bool
-- ^Evaluate whether a tile is the player.
isPlayer t = t == Player

-- =============================================================== --
-- Game and level restarting

-- Exported

getNxtLevel :: Game -> Game -> Game
getNxtLevel g0 g1 = g1 & T.items   .~ ( g0 ^. T.items )
                       & T.level   .~ ( succ $ g0 ^. T.level )
                       & T.oneups  .~ ( g0 ^. T.oneups )
                       & T.pwrmult .~ 1

restartLevel :: Game -> Game
-- ^Restarts the current level.
restartLevel g = g & T.status  .~ Running
                   & T.pacman  %~ resetPacMan
                   & T.ghosts  %~ map resetGhost
                   & T.pwrmult .~ 1
                   & T.oneups  %~ subtract 1

-- Unexported

resetPacMan :: PacMan -> PacMan
-- ^Resets player to origin position and direction.
resetPacMan p = let (pos0, dir0) = p ^. T.pstrt
                in  p & T.ppos .~ pos0
                      & T.pdir .~ dir0

resetGhost :: Ghost -> Ghost
-- ^Reset a ghost to its original position and direction.
resetGhost g = let (pos0, dir0) = g ^. T.gstrt
               in  g & T.gpos    .~ pos0
                     & T.gdir    .~ dir0
                     & T.gedible .~ False

-- =============================================================== --
-- Game status management

-- Exported

updateGame :: Game -> Game -> Game
updateGame g0 g1
    | levelFinished = g1 & T.status .~ LevelOver
    | otherwise     = checkPower g0 . checkCaptures g0 $ g1
    where levelFinished = g1 ^. T.npellets == 0

---------------------------------------------------------------------
-- Eating ghosts and getting captured by ghosts

-- Unexported

checkCaptures :: Game -> Game -> Game
checkCaptures g0 g1
    | null gsts = g1
    | ateGhost  = g1 & T.ghosts .~ eaten : uneaten
                     & T.items . T.gstscore %~ (+ 100 * pm)
                     & T.pwrmult %~ (*2)
    | moreLives = g1 & T.status .~ ReplayLvl
    | otherwise = g1 & T.status .~ GameOver
    where gsts     = ghostCapture g0 g1
          ateGhost = all ( ^. T.gedible ) gsts
          eaten    = resetGhost . head $ gsts
          uneaten  = filter (/= eaten) $ g1 ^. T.ghosts
          pm       = g1 ^. T.pwrmult
          moreLives = g1 ^. T.oneups > 0

ghostCapture :: Game -> Game -> [Ghost]
-- ^Return a list of all ghosts capturing or captured by player.
ghostCapture g0 g1 =
    [ y | (x,y) <- gs, isCapture (p0, p1) (x ^. T.gpos, y ^. T.gpos) ]
    where p0 = g0 ^. T.pacman . T.ppos
          p1 = g1 ^. T.pacman . T.ppos
          gs = zip ( g0 ^. T.ghosts ) ( g1 ^. T.ghosts )

isCapture :: (Point, Point) -> (Point, Point) -> Bool
isCapture (p0, p1) (g0, g1) = p1 == g1 || p1 == g0 && p0 == g1

---------------------------------------------------------------------
-- Dealing with the "powered" state after eating a power pellet

-- Unexported

checkPower :: Game -> Game -> Game
checkPower g0 g1
    | wasPowered g0 g1 = powerGame g1
    | wasDepowered g1  = depowerGame g1
    | otherwise        = g1

powerGame :: Game -> Game
powerGame g = let gsts = map ( set T.gedible True ) $ g ^. T.ghosts
              in  g & T.ghosts .~ gsts
                    & T.status .~ PwrRunning ( g ^. T.time )

depowerGame :: Game -> Game
depowerGame g = let gsts = map ( set T.gedible False ) $ g ^. T.ghosts
                in  g & T.ghosts  .~ gsts
                      & T.status  .~ Running
                      & T.pwrmult .~ 1

wasPowered :: Game -> Game -> Bool
wasPowered g0 g1 = t1 == PwrPellet
    where t1 = (g0 ^. T.maze) ! (g1 ^. T.pacman . T.ppos)

wasDepowered :: Game -> Bool
wasDepowered g = case g ^. T.status of
                      PwrRunning _ -> pwrTimeLeft g == 0
                      otherwise    -> False

pwrTimeLeft :: Game -> Int
pwrTimeLeft g
    | dt > 0    = dt
    | otherwise = 0
    where dt = case g ^. T.status of
                    PwrRunning t0 -> g ^. T.pwrtime - ( g ^. T.time - t0 )
                    otherwise     -> 0

-- =============================================================== --
-- Moving ghosts and player

---------------------------------------------------------------------
-- General utilities

-- Unexported

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

dirToShift :: Direction -> Point
-- ^Maps directions to single-tick displacements.
dirToShift West  = (0,-1)
dirToShift East  = (0, 1)
dirToShift North = (-1,0)
dirToShift South = (1, 0)

---------------------------------------------------------------------
-- Moving and updating the player

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
-- Moving and updating the ghosts

-- Exported

moveGhosts :: Game -> Game
moveGhosts g = let (gsts, r) = foldr (moveGhost g) start $ g ^. T.ghosts
                   start        = ([], g ^. T.rgen )
               in  g & T.ghosts .~ gsts
                     & T.rgen   .~ r

tileGhosts :: Game -> [(Point, Tile)]
tileGhosts g = [ (gst ^. T.gpos, tileGhost g gst) | gst <- g ^. T.ghosts ]

-- Unexported

tileGhost :: Game -> Ghost -> Tile
tileGhost g gst
    | isBlueGhost  = BlueGhost
    | isWhiteGhost = WhiteGhost
    | otherwise    = gst ^. T.gname
    where isBlueGhost  = gst ^. T.gedible && isBlue (pwrTimeLeft g) g
          isWhiteGhost = gst ^. T.gedible

isBlue :: Int -> Game -> Bool
isBlue tlft g
    | tlft >= half = True
    | otherwise    = even . quot tlft $ ( g ^. T.dtime )
    where half = quot ( g ^. T.pwrtime ) 2

moveGhost :: Game -> Ghost -> ([Ghost], StdGen) -> ([Ghost], StdGen)
moveGhost g gst0 (gsts, r0) = (gst1:gsts, r1)
    where p0       = gst0 ^. T.gpos
          m        = g ^. T.maze
          (r1, ds) = randomDirections r0 dirs
          ps       = [ (d, getNxtPos p0 (m ! p0) d) | d <- ds ]
          (d1, p1) = head . dropWhile (not . isFree m . snd) $ ps
          gst1     = gst0 & T.gpos .~ p1 & T.gdir .~ d1
          dirs     = [North, South, East, West] ++ replicate 20 pdir
          pdir     = case chase g gst0 of
                          Just d  -> d
                          Nothing -> gst0 ^. T.gdir

noWalls :: Int -> Int -> V.Vector Tile -> Bool
noWalls x y ts
    | x < y     = not . V.any (isWall) . V.slice x d $ ts
    | x > y     = not . V.any (isWall) . V.slice y d $ ts
    | otherwise = False
    where d = abs $ x - y

chase :: Game -> Ghost -> Maybe Direction
chase g gst
    | pc == gc && pr < gr && noWalls gr pr cPath = Just North
    | pc == gc && pr > gr && noWalls gr pr cPath = Just South
    | pr == gr && pc < gc && noWalls gc pc rPath = Just West
    | pr == gr && pc > gc && noWalls gc pc rPath = Just East
    | otherwise                                  = Nothing
    where (gr, gc) = gst ^. T.gpos
          (pr, pc) = g ^. T.pacman . T.ppos
          rPath    = M.getRow gr ( g ^. T.maze )
          cPath    = M.getCol gc ( g ^. T.maze )

randomDirections :: StdGen -> [Direction] -> (StdGen, [Direction])
randomDirections r0 [] = (r0, [])
randomDirections r0 ds0 = (r, d:ds)
    where (k,r1)  = randomR (0, length ds0 - 1) r0
          d       = ds0 !! k
          ds1     = delete d . nub $ ds0
          (r,ds)  = randomDirections r1 ds1
