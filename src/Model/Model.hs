module Model.Model
    ( -- Game and level restarting
      restartLevel
    -- Running ame state updating
    , updateGame
    -- Time management
    , updateClock
    , updateTimePaused
    , updateTime
    -- Moving and updating the player
    , movePlayer
    -- Moving the ghosts
    , moveGhosts
    ) where

import qualified Data.Matrix as M
import qualified Model.Types as T
import Data.List                    ( (\\), foldl', sort    )
import Lens.Micro                   ( (&), (^.), (.~), (%~) )
import Data.Matrix                  ( (!)                   )
import System.Random                ( randomR
                                    , StdGen                )
import Model.Utilities              ( edibleGhostWaitTime
                                    , ghostWaitTime
                                    , highScore
                                    , moveFrom
                                    , noWalls
                                    , pathBetween
                                    , playerScore
                                    , playerWaitTime
                                    , revDirection
                                    , scoreFruit
                                    , scoreMessage
                                    , tickPeriod            )
import Model.Types                  ( Direction     (..)
                                    , Fruit         (..)
                                    , FruitName     (..)
                                    , Game          (..)
                                    , Ghost         (..)
                                    , Items         (..)
                                    , GhostState    (..)
                                    , Maze          (..)
                                    , Message       (..)
                                    , Mode          (..)
                                    , PacMan        (..)
                                    , Point         (..)
                                    , Score         (..)
                                    , Tile          (..)
                                    , Time          (..)    )

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Pure functions for managing game state

-- =============================================================== --
-- Game and level restarting

-- Exported

restartLevel :: Game -> Game
-- ^Restarts the current level after player is captured by a ghost.
restartLevel g = g & T.mode  .~ Running
                   & T.pacman  %~ resetPacMan
                   & T.ghosts  %~ map resetGhost
                   & T.pwrmult .~ 2
                   & T.oneups  %~ subtract 1

-- Unexported

resetPacMan :: PacMan -> PacMan
resetPacMan p = let (pos0, dir0) = p ^. T.pstrt
                in  p & T.ppos .~ pos0
                      & T.pdir .~ dir0

resetGhost :: Ghost -> Ghost
resetGhost g = let (pos0, dir0) = g ^. T.gstrt
               in  g & T.gpos      .~ pos0
                     & T.gdir      .~ dir0
                     & T.gstate    .~ Normal
                     & T.gpathback .~ []

-- =============================================================== --
-- Running game state updating

-- Exported

updateGame :: Game -> Game -> Game
updateGame gm0 gm1
    | levelFinished = gm1 & T.mode .~ LevelOver
    | otherwise     = runUpdate gm0 gm1
    where levelFinished = gm1 ^. T.npellets == 0

-- Unexported

runUpdate :: Game -> Game -> Game
runUpdate gm0 gm1 = updatePower . updateFruit . updateCaptures gm0 $ gm1

---------------------------------------------------------------------
-- Time management

-- Exported

updateClock :: Time -> Game -> Game
-- ^Does nothing besides keep the game clock ticking. This function
-- is only needed when the game is not running (e.g., when the player
-- is entering their name for a new high score).
updateClock t gm = let dt = t - gm ^. T.time
                   in  gm & T.time .~ t & T.dtime .~ dt

updateTimePaused :: Time -> Game -> Game
-- ^Keeps all time variables up to date during a paused game state.
updateTimePaused t gm =
    let dt = t - gm ^. T.time
    in  gm & T.time .~ t
           & T.dtime .~ dt
           & T.pacman . T.ptlast %~ (+dt)
           & T.ghosts %~ map ( \ g -> g & T.gtlast %~ (+dt) )

updateTime :: Time -> Game -> Game
-- ^Keeps all time variables up to date during a running game state.
updateTime t = updateMessageTime
                 . updatePowerTime
                 . updateFruitTimes
                 . updateClock t

-- Unexported

updatePowerTime :: Game -> Game
updatePowerTime gm = gm & T.mode %~ go
    where go (PwrRunning t0) = PwrRunning (t0 - gm ^. T.dtime)
          go x               = x

updateFruitTimes :: Game -> Game
updateFruitTimes gm = gm & T.fruit %~ fmap go
    where dt = gm ^. T.dtime
          go frt | frt ^. T.fdelay > 0    = frt & T.fdelay    %~ subtract dt
                 | frt ^. T.fduration > 0 = frt & T.fduration %~ subtract dt
                 | otherwise              = frt

updateMessageTime :: Game -> Game
updateMessageTime gm = gm & T.msg %~ go
    where dt = gm ^. T.dtime
          go NoMessage     = NoMessage
          go (Message s t) | t > 0     = Message s (t - dt)
                           | otherwise = NoMessage

---------------------------------------------------------------------
-- Eating ghosts and getting captured by ghosts

-- Unexported

updateCaptures :: Game -> Game -> Game
updateCaptures gm0 gm1
    | null captures = gm1
    | ateGhost      = eatGhosts gm1 captures
    | moreLives     = gm1 & T.mode .~ ReplayLvl
    | newHighScore  = gm1 & T.mode .~ NewHighScore
    | otherwise     = gm1 & T.mode .~ GameOver
    where captures     = ghostCaptures gm0 gm1
          ateGhost     = all ( (== Edible) . (^. T.gstate) ) captures
          moreLives    = gm1 ^. T.oneups > 0
          newHighScore = playerScore gm1 > highScore gm1

eatGhosts :: Game -> [Ghost] -> Game
eatGhosts gm gs = let eaten   = map (eatGhost gm) gs
                      uneaten = ( gm ^. T.ghosts ) \\ eaten
                      nEaten  = length eaten
                      scores  = scoreCaptures (gm ^. T.pwrmult) nEaten
                      score   = sum scores
                  in  gm & T.ghosts .~ sort ( eaten ++ uneaten )
                         & T.items . T.gstscores %~ addGhostScores scores
                         & T.pwrmult %~ (* (2 ^ nEaten) )
                         & T.msg .~ if nEaten == 1
                                       then scoreMessage "Ghost" score
                                       else let msg = show nEaten ++ " Ghosts"
                                            in  scoreMessage msg score

ghostCaptures :: Game -> Game -> [Ghost]
-- ^Return a list of all ghosts capturing or captured by player. This
-- requires that the list of ghosts always remains in sorted order.
ghostCaptures gm0 gm1 = let p0 = gm0 ^. T.pacman . T.ppos
                            p1 = gm1 ^. T.pacman . T.ppos
                            gs = zip ( gm0 ^. T.ghosts ) ( gm1 ^. T.ghosts )
                        in  [ g1 | (g0, g1) <- gs
                            , g0 ^. T.gstate /= EyesOnly
                            , isCapture (p0, p1) (g0 ^. T.gpos, g1 ^. T.gpos) ]

isCapture :: (Point, Point) -> (Point, Point) -> Bool
isCapture (p0, p1) (g0, g1) = p1 == g1 || p1 == g0 && p0 == g1

eatGhost :: Game -> Ghost -> Ghost
eatGhost gm g = let m  = gm ^. T.maze
                    p0 = g ^. T.gpos
                    p1 = fst $ g ^. T.gstrt
                in g & T.gstate    .~ EyesOnly
                     & T.gpathback .~ pathBetween m p0 p1

scoreCaptures :: Int -> Int -> [Score]
scoreCaptures m n = map ( \ k -> 100 * m * 2 ^ k ) [0..n-1]

addGhostScores :: [Score] -> [(Score, Int)] -> [(Score, Int)]
addGhostScores toAdd scores = foldl' go scores toAdd
    where go []         x             = [(x, 1)]
          go ((y,n):ys) x | x == y    = (y, n+1) : ys
                          | otherwise = (y, n  ) : go ys x

---------------------------------------------------------------------
-- Updating the appearance and disappearance of fruit

-- Unexported

updateFruit :: Game -> Game
updateFruit gm = case gm ^. T.fruit of
                      Nothing  -> gm
                      Just frt -> checkFruitCapture gm frt

checkFruitCapture :: Game -> Fruit -> Game
checkFruitCapture gm frt
    | isWaiting = gm
    | capture   = eatFruit gm frt
    | isVisible = gm
    | otherwise = gm & T.fruit .~ Nothing
    where isWaiting = frt ^. T.fdelay > 0
          isVisible = frt ^. T.fduration > 0
          capture   = frt ^. T.fpos == gm ^. T.pacman . T.ppos

eatFruit :: Game -> Fruit -> Game
eatFruit gm frt = let name = frt ^. T.fname
                  in  gm & T.fruit .~ Nothing
                         & T.msg   .~ scoreMessage (show name) (scoreFruit name)
                         & T.items %~ addFruitToItems name

addFruitToItems :: FruitName -> Items -> Items
addFruitToItems n items = items & T.fruits %~ go
    where go []         = [(n, 1)]
          go ((f,c):fs) | f == n    = (f, c + 1) : fs
                        | otherwise = (f, c) : go fs

---------------------------------------------------------------------
-- Dealing with the "powered" state after eating a power pellet

-- Unexported

updatePower :: Game -> Game
updatePower gm = case gm ^. T.mode of
                      PwrRunning t -> managePower t gm
                      otherwise    -> gm

managePower :: Time -> Game -> Game
managePower t gm
    | t > 0     = gm
    | otherwise = depoweredGame
    where depoweredGame = gm & T.ghosts  %~ map makeInedible
                             & T.mode    .~ Running
                             & T.pwrmult .~ 2

makeInedible :: Ghost -> Ghost
makeInedible g = case g ^. T.gstate of
                      Edible    -> g & T.gstate .~ Normal
                      otherwise -> g

-- =============================================================== --
-- Moving ghosts and player

---------------------------------------------------------------------
-- Moving and updating the player

movePlayer :: Game -> Game
movePlayer gm
    | isPlayerWaiting gm = gm
    | otherwise          = let p0  = gm ^. T.pacman . T.ppos
                               d0  = gm ^. T.pacman . T.pdir
                               m   = gm ^. T.maze
                               p1  = moveFrom m p0 d0
                               gm' = gm & T.pacman . T.ppos .~ p1
                                        & T.pacman . T.ptlast .~ (gm ^. T.time)
                           in  case m ! p1 of
                                    PwrPellet -> eatPwrPellet gm p1
                                    Pellet    -> eatPellet gm p1
                                    Wall _    -> gm
                                    OneWay d  -> if d == d0 then gm' else gm
                                    otherwise -> gm'

-- Unexported

isPlayerWaiting :: Game -> Bool
isPlayerWaiting gm = dt < playerWaitTime
    where dt = gm ^. T.time - gm  ^. T.pacman . T.ptlast

eatPellet :: Game -> Point -> Game
eatPellet gm p = gm & T.pacman . T.ppos .~ p
                    & T.pacman . T.ptlast .~ ( gm ^. T.time )
                    & T.maze %~ M.setElem Empty p
                    & T.npellets %~ pred
                    & T.items . T.pellets %~ succ

eatPwrPellet :: Game -> Point -> Game
eatPwrPellet gm p = gm & T.pacman . T.ppos .~ p
                       & T.pacman . T.ptlast .~ ( gm ^. T.time )
                       & T.maze %~ M.setElem Empty p
                       & T.npellets %~ pred
                       & T.items . T.ppellets %~ succ
                       & T.msg .~ scoreMessage "Power Pellet" 50
                       & T.ghosts %~ map makeEdible
                       & T.mode .~ PwrRunning (gm ^. T.pwrtime)

makeEdible :: Ghost -> Ghost
makeEdible g = case g ^. T.gstate of
                    EyesOnly  -> g
                    otherwise -> g & T.gstate .~ Edible

---------------------------------------------------------------------
-- Moving the ghosts

-- Exported

moveGhosts :: Game -> Game
moveGhosts gm = let (gs, r) = foldr ( moveGhost gm ) start $ gm ^. T.ghosts
                    start   = ( [], gm ^. T.rgen )
                in  gm & T.ghosts .~ gs
                       & T.rgen   .~ r

-- Unexported

moveGhost :: Game -> Ghost -> ([Ghost], StdGen) -> ([Ghost], StdGen)
moveGhost gm g0 (gs, r0) = case g0 ^. T.gstate of
                                EyesOnly  -> moveEyes g0 (gs, r0)
                                otherwise -> moveWholeGhost gm g0 (gs, r0)

moveEyes :: Ghost -> ([Ghost], StdGen) -> ([Ghost], StdGen)
moveEyes g0 (gs,r) = (g1:gs, r)
    where g1 = case g0 ^. T.gpathback of
                    []     -> resetGhost g0
                    (p:ps) -> g0 & T.gpos      .~ p
                                 & T.gpathback .~ ps

moveWholeGhost :: Game -> Ghost -> ([Ghost], StdGen) -> ([Ghost], StdGen)
moveWholeGhost gm g0 (gs, r0)
    | isGhostWaiting gm g0 = (g0:gs, r0)
    | otherwise            = (g1:gs, r1)
    where p0      = g0 ^. T.gpos
          m       = gm ^. T.maze
          atStart = g0 ^. T.gpos == fst ( g0 ^. T.gstrt )
          bias    = if atStart then 0 else 20
          (r1,ds) = proposeDirections gm g0 r0 bias
          (d1,p1) = chooseDirection gm g0 ds
          g1      = g0 & T.gpos   .~ p1
                       & T.gdir   .~ d1
                       & T.gtlast .~ gm ^. T.time

isGhostWaiting :: Game -> Ghost -> Bool
isGhostWaiting gm g = let dt = gm ^. T.time - g ^. T.gtlast
                      in  case g ^. T.gstate of
                               Normal    -> dt < ghostWaitTime
                               Edible    -> dt < edibleGhostWaitTime
                               EyesOnly  -> dt < ghostWaitTime

chooseDirection :: Game -> Ghost -> [Direction] -> (Direction, Point)
chooseDirection gm g []     = (g ^. T.gdir, g ^. T.gpos)
chooseDirection gm g (d:ds) =
    let m   = gm ^. T.maze
        p   = moveFrom m (g ^. T.gpos) d
        nxt = chooseDirection gm g ds
    in  case m ! p of
             Wall _     -> nxt
             OneWay owd -> if d == owd then (d, p) else nxt
             otherwise  -> (d, p)

proposeDirections :: Game -> Ghost -> StdGen -> Int -> (StdGen, [Direction])
proposeDirections gm g r bias = randomDirections r ds
    where ds = [North, South, East, West] ++ replicate bias pd
          pd = case toPacMan gm g of
                    Nothing -> g ^. T.gdir
                    Just d  -> if g ^. T.gstate == Edible
                                  then revDirection d
                                  else d

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
    where (k,r1) = randomR (0, length ds0 - 1) r0
          d      = ds0 !! k
          (r,ds) = randomDirections r1 . filter (/= d) $ ds0
