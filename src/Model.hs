module Model
    ( isGhost
    , isPellet
    , isPlayer
    , isWall
    , getNxtLevel
    , restartLevel
    , updateGame
    , playerWaitTime
    , movePlayer
    , moveGhosts
    , tileGhosts
    , ghostWaitTime
    , edibleGhostWaitTime
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
                       & T.pwrmult .~ 2

restartLevel :: Game -> Game
-- ^Restarts the current level.
restartLevel g = g & T.status  .~ Running
                   & T.pacman  %~ resetPacMan
                   & T.ghosts  %~ map resetGhost
                   & T.pwrmult .~ 2
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
    | otherwise     = checkPower g0 . checkCaptures g0 . updateMessage $ g1
    where levelFinished = g1 ^. T.npellets == 0

---------------------------------------------------------------------
-- Eating ghosts and getting captured by ghosts

-- Unexported

updateMessage :: Game -> Game
updateMessage gm = go $ gm ^. T.msg
    where go Nothing      = gm
          go (Just (s,t)) | t < 0     = gm & T.msg .~ Nothing
                          | otherwise = let t' = t - gm ^. T.dtime
                                        in  gm & T.msg .~ Just (s, t')

checkCaptures :: Game -> Game -> Game
checkCaptures g0 g1
    | null gsts = g1
    | ateGhost  = g1 & T.ghosts .~ eaten : uneaten
                     & T.items . T.gstscore %~ (+ dscore)
                     & T.pwrmult %~ (*2)
                     & T.msg .~ Just ("Ghost eaten! +" ++ show dscore, 3000000)
    | moreLives = g1 & T.status .~ ReplayLvl
    | otherwise = g1 & T.status .~ GameOver
    where gsts      = ghostCapture g0 g1
          ateGhost  = all ( ^. T.gedible ) gsts
          eaten     = resetGhost . head $ gsts
          uneaten   = filter (/= eaten) $ g1 ^. T.ghosts
          dscore    = 100 * ( g1 ^. T.pwrmult )
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
                      & T.pwrmult .~ 2

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

playerWaitTime :: Int
playerWaitTime = 225000

movePlayer :: Game -> Game
movePlayer g
    | isWaiting       = g
    | isWall t1       = g
    | t1 == PwrPellet = g & T.pacman . T.ppos .~ p1
                          & T.pacman . T.ptlast .~ ( g ^. T.time )
                          & T.maze %~ M.setElem Empty p1
                          & T.npellets %~ pred
                          & T.items . T.ppellets %~ succ
                          & T.msg .~ Just ("Power Pellet! +50", 3000000)
    | t1 == Pellet    = g & T.pacman . T.ppos .~ p1
                          & T.pacman . T.ptlast .~ ( g ^. T.time )
                          & T.maze %~ M.setElem Empty p1
                          & T.npellets %~ pred
                          & T.items . T.pellets %~ succ
    | otherwise       = g & T.pacman . T.ppos .~ p1
    where isWaiting = g ^. T.time - g ^. T.pacman . T.ptlast < playerWaitTime
          p0 = g ^. T.pacman . T.ppos
          m0 = g ^. T.maze
          p1 = getNxtPos p0 (m0 ! p0) $ g ^. T.pacman . T.pdir
          t1 = m0 ! p1

---------------------------------------------------------------------
-- Moving and updating the ghosts

-- Exported

tileGhosts :: Game -> [(Point, Tile)]
tileGhosts gm = [ (g ^. T.gpos, tileGhost gm g) | g <- gm ^. T.ghosts ]

ghostWaitTime :: Int
ghostWaitTime = 225000

edibleGhostWaitTime :: Int
edibleGhostWaitTime = 2 * ghostWaitTime

-- Unexported

tileGhost :: Game -> Ghost -> Tile
tileGhost gm g
    | isBlueGhost  = BlueGhost
    | isWhiteGhost = WhiteGhost
    | otherwise    = g ^. T.gname
    where isBlueGhost  = g ^. T.gedible && isBlue (pwrTimeLeft gm) gm
          isWhiteGhost = g ^. T.gedible

isBlue :: Int -> Game -> Bool
isBlue tlft gm
    | tlft >= half = True
    | otherwise    = even . quot tlft $ ( gm ^. T.dtime )
    where half = quot ( gm ^. T.pwrtime ) 2

moveGhosts :: Game -> Game
moveGhosts gm = let (gs, r) = foldr ( moveGhost gm ) start $ gm ^. T.ghosts
                    start   = ( [], gm ^. T.rgen )
                in  gm & T.ghosts .~ gs
                       & T.rgen   .~ r

moveGhost :: Game -> Ghost -> ([Ghost], StdGen) -> ([Ghost], StdGen)
moveGhost gm g0 (gs, r0)
    | isWaiting = (g0:gs, r0)
    | otherwise = (g1:gs, r1)
    where dt        = gm ^. T.time - g0 ^. T.gtlast
          isWaiting | g0 ^. T.gedible = dt < edibleGhostWaitTime
                    | otherwise       = dt < ghostWaitTime
          p0        = g0 ^. T.gpos
          m         = gm ^. T.maze
          (r1,ds)   = proposeDirections gm g0 r0
          ps        = [ (d, getNxtPos p0 (m ! p0) d) | d <- ds ]
          (d1,p1)   = head . filter (isFree m . snd) $ ps
          g1        = g0 & T.gpos   .~ p1
                         & T.gdir   .~ d1
                         & T.gtlast .~ gm ^. T.time

proposeDirections :: Game -> Ghost -> StdGen -> (StdGen, [Direction])
proposeDirections gm g r = randomDirections r ds
    where ds = [North, South, East, West] ++ replicate 20 pd
          pd = case toPacMan gm g of
                    Nothing -> g ^. T.gdir
                    Just d  -> if g ^. T.gedible
                               then runAway d
                               else d

runAway :: Direction -> Direction
runAway North = South
runAway South = North
runAway West  = East
runAway East  = West

noWalls :: Int -> Int -> V.Vector Tile -> Bool
noWalls x y ts
    | x < y     = not . V.any (isWall) . V.slice x d $ ts
    | x > y     = not . V.any (isWall) . V.slice y d $ ts
    | otherwise = False
    where d = abs $ x - y

toPacMan :: Game -> Ghost -> Maybe Direction
toPacMan g gst
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
