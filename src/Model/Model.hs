module Model.Model
    ( restartLevel
    , updateGame
    , movePlayer
    , moveGhosts
    , tileGhosts
    ) where

import qualified Data.Matrix as M
-- import qualified Data.Vector as V
import qualified Model.Types as T
import Lens.Micro                   ( (&), (^.), (.~), (%~), set    )
import Data.Matrix                  ( (!)                           )
import Data.List                    ( delete
                                    , nub                           )
import System.Random                ( StdGen
                                    , randomR                       )
import Model.Utilities              ( isWall
                                    , isFree
                                    , noWalls
                                    , powerTimeLeft
                                    , playerWaitTime
                                    , ghostWaitTime
                                    , edibleGhostWaitTime
                                    , revDirection
                                    , moveFrom
                                    , pathBetween                   )
import Model.Types                  ( Tile          (..)
                                    , Game          (..)
                                    , Status        (..)
                                    , Point         (..)
                                    , Maze          (..)
                                    , Direction     (..)
                                    , PacMan        (..)
                                    , Ghost         (..)
                                    , GhostState    (..)            )

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Pure functions for managing game state

-- =============================================================== --
-- Game and level restarting

-- Exported

restartLevel :: Game -> Game
-- ^Restarts the current level after player is captured by a ghost.
restartLevel g = g & T.status  .~ Running
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
-- Game status updating

-- Exported

updateGame :: Game -> Game -> Game
updateGame g0 g1
    | levelFinished = g1 & T.status .~ LevelOver
    | otherwise     = updatePower . updateCaptures g0 . updateMessage $ g1
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

updateCaptures :: Game -> Game -> Game
updateCaptures g0 g1
    | null gsts = g1
    | ateGhost  = g1 & T.ghosts .~ eaten : uneaten
                     & T.items . T.gstscore %~ (+ dscore)
                     & T.pwrmult %~ (*2)
                     & T.msg .~ Just ("Ghost eaten! +" ++ show dscore, 3000000)
    | moreLives = g1 & T.status .~ ReplayLvl
    | otherwise = g1 & T.status .~ GameOver
    where gsts      = ghostCapture g0 g1
          ateGhost  = all ( (== Edible) . (^. T.gstate) ) gsts
          eaten     = eatGhost g0 . head $ gsts
          uneaten   = filter (/= eaten) $ g0 ^. T.ghosts
          dscore    = 100 * ( g1 ^. T.pwrmult )
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
-- Dealing with the "powered" state after eating a power pellet

-- Unexported

updatePower :: Game -> Game
updatePower gm
    | isPowered = gm
    | otherwise = gm & T.ghosts  %~ map makeInedible
                     & T.status  .~ Running
                     & T.pwrmult .~ 2
    where isPowered = case gm ^. T.status of
                           PwrRunning _ -> powerTimeLeft gm > 0
                           otherwise    -> False

-- =============================================================== --
-- Moving ghosts and player

---------------------------------------------------------------------
-- Moving and updating the player

movePlayer :: Game -> Game
movePlayer gm
    | isPlayerWaiting gm = gm
    | otherwise          = let p0 = gm ^. T.pacman . T.ppos
                               m  = gm ^. T.maze
                               p1 = moveFrom m p0 $ gm ^. T.pacman . T.pdir
                           in  case m ! p1 of
                                    PwrPellet -> eatPwrPellet gm p1
                                    Pellet    -> eatPellet gm p1
                                    Wall _    -> gm
                                    otherwise -> gm & T.pacman . T.ppos .~ p1

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
                       & T.msg .~ Just ("Power Pellet! +50", 3000000) 
                       & T.ghosts %~ map makeEdible
                       & T.status .~ PwrRunning ( gm ^. T.time )

isPlayerWaiting :: Game -> Bool
isPlayerWaiting gm = dt < playerWaitTime
    where dt = gm ^. T.time - gm  ^. T.pacman . T.ptlast

---------------------------------------------------------------------
-- Tiling the ghosts

-- Exported

tileGhosts :: Game -> [(Point, Tile)]
tileGhosts gm = [ (g ^. T.gpos, tileGhost gm g) | g <- gm ^. T.ghosts ]

-- Unexported

tileGhost :: Game -> Ghost -> Tile
tileGhost gm g = case g ^. T.gstate of
                      Edible    -> tileEdibleGhost gm g
                      EyesOnly  -> GhostEyes
                      otherwise -> g ^. T.gname

tileEdibleGhost :: Game -> Ghost -> Tile
tileEdibleGhost gm g
    | trem >= half = BlueGhost
    | isWhite      = WhiteGhost
    | otherwise    = BlueGhost
    where trem    = powerTimeLeft gm
          half    = quot ( gm ^. T.pwrtime ) 2
          isWhite = odd . quot trem $ gm ^. T.dtime

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
          (r1,ds) = proposeDirections gm g0 r0
          ps      = [ (d, moveFrom m p0 d) | d <- ds ]
          (d1,p1) = head . filter (isFree m . snd) $ ps
          g1      = g0 & T.gpos   .~ p1
                       & T.gdir   .~ d1
                       & T.gtlast .~ gm ^. T.time

isGhostWaiting :: Game -> Ghost -> Bool
isGhostWaiting gm g = let dt = gm ^. T.time - g ^. T.gtlast
                      in  case g ^. T.gstate of
                               Normal    -> dt < ghostWaitTime
                               Edible    -> dt < edibleGhostWaitTime
                               EyesOnly  -> dt < ghostWaitTime

proposeDirections :: Game -> Ghost -> StdGen -> (StdGen, [Direction])
proposeDirections gm g r = randomDirections r ds
    where ds = [North, South, East, West] ++ replicate 20 pd
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
    where (k,r1)  = randomR (0, length ds0 - 1) r0
          d       = ds0 !! k
          ds1     = delete d . nub $ ds0
          (r,ds)  = randomDirections r1 ds1
