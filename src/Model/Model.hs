module Model.Model
    ( restartLevel
    , updateGame
    , updateTime
    , movePlayer
    , moveGhosts
    ) where

import qualified Data.Matrix as M
import qualified Model.Types as T
import Lens.Micro                   ( (&), (^.), (.~), (%~)         )
import Data.Matrix                  ( (!)                           )
import System.Random                ( StdGen
                                    , randomR                       )
import Model.Utilities              ( tickPeriod
                                    , noWalls
                                    , powerTimeLeft
                                    , playerWaitTime
                                    , ghostWaitTime
                                    , messageTime
                                    , edibleGhostWaitTime
                                    , scoreFruit
                                    , revDirection
                                    , moveFrom
                                    , pathBetween                   )
import Model.Types                  ( Tile          (..)
                                    , Time          (..)
                                    , Game          (..)
                                    , Mode          (..)
                                    , Point         (..)
                                    , Maze          (..)
                                    , Direction     (..)
                                    , PacMan        (..)
                                    , Ghost         (..)
                                    , Fruit         (..)
                                    , FruitName     (..)
                                    , Items         (..)
                                    , GhostState    (..)            )

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
-- Game state updating

updateGame :: Game -> Game -> Game
updateGame g0 g1
    | levelFinished = g1 & T.mode .~ LevelOver
    | otherwise     = runUpdate g0 g1
    where levelFinished = g1 ^. T.npellets == 0

updateTime :: Time -> Game -> Game
updateTime t gm = let dt = t - gm ^. T.time
                  in  gm & T.time  .~ t
                         & T.dtime .~ dt

---------------------------------------------------------------------
-- Eating ghosts and getting captured by ghosts

-- Unexported

runUpdate :: Game -> Game -> Game
runUpdate g0 g1 = updatePower
                  . updateFruit
                  . updateCaptures g0
                  . updateMessage $ g1

updateMessage :: Game -> Game
updateMessage gm = go $ gm ^. T.msg
    where go Nothing      = gm
          go (Just (s,t)) | t < 0     = gm & T.msg .~ Nothing
                          | otherwise = let t' = t - gm ^. T.dtime
                                        in  gm & T.msg .~ Just (s, t')

updateCaptures :: Game -> Game -> Game
updateCaptures g0 g1
    | null gsts = g1
    | ateGhost  = g1 & T.ghosts .~ eaten : uneaten
                     & T.items . T.gstscore %~ (+ ds)
                     & T.pwrmult %~ (*2)
                     & T.msg .~ Just ("Ghost +" ++ show ds ++ "!", messageTime)
    | moreLives = g1 & T.mode .~ ReplayLvl
    | otherwise = g1 & T.mode .~ GameOver
    where gsts      = ghostCapture g0 g1
          ateGhost  = all ( (== Edible) . (^. T.gstate) ) gsts
          eaten     = eatGhost g0 . head $ gsts
          uneaten   = filter (/= eaten) $ g0 ^. T.ghosts
          ds        = 100 * ( g1 ^. T.pwrmult )
          moreLives = g1 ^. T.oneups > 0

eatGhost :: Game -> Ghost -> Ghost
eatGhost gm g = let m  = gm ^. T.maze
                    p0 = g ^. T.gpos
                    p1 = fst $ g ^. T.gstrt
                in g & T.gstate    .~ EyesOnly
                     & T.gpathback .~ pathBetween m p0 p1

ghostCapture :: Game -> Game -> [Ghost]
-- ^Return a list of all ghosts capturing or captured by player.
ghostCapture gm0 gm1 = let p0 = gm0 ^. T.pacman . T.ppos
                           p1 = gm1 ^. T.pacman . T.ppos
                           gs = zip ( gm0 ^. T.ghosts ) ( gm1 ^. T.ghosts )
                     in  [ g1 | (g0, g1) <- gs
                             , g0 ^. T.gstate /= EyesOnly
                             , isCapture (p0, p1) (g0 ^. T.gpos, g1 ^. T.gpos) ]

isCapture :: (Point, Point) -> (Point, Point) -> Bool
isCapture (p0, p1) (g0, g1) = p1 == g1 || p1 == g0 && p0 == g1

makeEdible :: Ghost -> Ghost
makeEdible g = case g ^. T.gstate of
                    EyesOnly  -> g
                    otherwise -> g & T.gstate .~ Edible

makeInedible :: Ghost -> Ghost
makeInedible g = case g ^. T.gstate of
                      Edible    -> g & T.gstate .~ Normal
                      otherwise -> g

---------------------------------------------------------------------
-- Updating the appearance and disappearance of fruit

-- Unexported

updateFruit :: Game -> Game
updateFruit gm = go (gm ^. T.fruit)
    where go Nothing                 = gm
          go (Just frt)
              | isWaiting = gm & T.fruit .~ Just stepWaiting
              | capture   = fruitEaten
              | isVisible = gm & T.fruit .~ Just stepVisible
              | otherwise = gm & T.fruit .~ Nothing
              where isWaiting   = frt ^. T.fdelay > 0
                    stepWaiting = frt & T.fdelay %~ adjustTime
                    isVisible   = frt ^. T.fduration > 0
                    stepVisible = frt & T.fduration %~ adjustTime
                    capture     = frt ^. T.fpos == gm ^. T.pacman . T.ppos
                    adjustTime  = subtract (gm ^. T.dtime)
                    fruitEaten  = let fnm = frt ^. T.fname
                                      scr = scoreFruit fnm
                                      msg = "Fruit +" ++ show scr ++ "!"
                                  in  gm & T.fruit .~ Nothing
                                         & T.msg   .~ Just (msg, messageTime)
                                         & T.items %~ addFruitToItems fnm

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
                      PwrRunning _ -> managePower gm
                      otherwise    -> gm

managePower :: Game -> Game
managePower gm
    | powerTimeLeft gm > 0 = gm
    | otherwise            = depoweredGame
    where depoweredGame = gm & T.ghosts  %~ map makeInedible
                             & T.mode  .~ Running
                             & T.pwrmult .~ 2

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
                               p1  = moveFrom m p0 $ gm ^. T.pacman . T.pdir
                               gm' = gm & T.pacman . T.ppos .~ p1
                           in  case m ! p1 of
                                    PwrPellet -> eatPwrPellet gm p1
                                    Pellet    -> eatPellet gm p1
                                    Wall _    -> gm
                                    OneWay d  -> if d == d0 then gm' else gm
                                    otherwise -> gm'

-- Unexported

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
                       & T.msg .~ Just ("Power Pellet +50!", messageTime)
                       & T.ghosts %~ map makeEdible
                       & T.mode .~ PwrRunning ( gm ^. T.time )

isPlayerWaiting :: Game -> Bool
isPlayerWaiting gm = dt < playerWaitTime
    where dt = gm ^. T.time - gm  ^. T.pacman . T.ptlast

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
