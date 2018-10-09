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
    | otherwise     = updatePower g0 . updateCaptures g0 . updateMessage $ g1
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
          ateGhost  = all isEdible gsts
          eaten     = eatGhost g0 . head $ gsts
          uneaten   = filter (/= eaten) $ g0 ^. T.ghosts
          dscore    = 100 * ( g1 ^. T.pwrmult )
          moreLives = g1 ^. T.oneups > 0

eatGhost :: Game -> Ghost -> Ghost
eatGhost gm g = let m = gm ^. T.maze
                    p0 = g ^. T.gpos
                    p1 = fst $ g ^. T.gstrt
                in g & T.gstate    .~ EyesOnly
                     & T.gpathback .~ pathBetween m p0 p1

ghostCapture :: Game -> Game -> [Ghost]
-- ^Return a list of all ghosts capturing or captured by player.
ghostCapture g0 g1 = let p0 = g0 ^. T.pacman . T.ppos
                         p1 = g1 ^. T.pacman . T.ppos
                         gs = zip ( g0 ^. T.ghosts ) ( g1 ^. T.ghosts )
                     in  [ y | (x,y) <- gs
                             , not . isEyesOnly $ x
                             , isCapture (p0, p1) (x ^. T.gpos, y ^. T.gpos) ]

isCapture :: (Point, Point) -> (Point, Point) -> Bool
isCapture (p0, p1) (g0, g1) = p1 == g1 || p1 == g0 && p0 == g1

makeEdible :: Ghost -> Ghost
makeEdible g = case g ^. T.gstate of
                    EyesOnly  -> g
                    otherwise -> g & T.gstate .~ Edible

isEdible :: Ghost -> Bool
isEdible g = g ^. T.gstate == Edible

isEyesOnly :: Ghost -> Bool
isEyesOnly g = g ^. T.gstate == EyesOnly

---------------------------------------------------------------------
-- Dealing with the "powered" state after eating a power pellet

-- Unexported

updatePower :: Game -> Game -> Game
updatePower g0 g1
    | wasPowered g0 g1 = powerGame g1
    | wasDepowered g1  = depowerGame g1
    | otherwise        = g1

powerGame :: Game -> Game
powerGame g = let gsts = map makeEdible $ g ^. T.ghosts
              in  g & T.ghosts .~ gsts
                    & T.status .~ PwrRunning ( g ^. T.time )

depowerGame :: Game -> Game
depowerGame gm = let go g = if isEdible g then set T.gstate Normal g else g
                     gs   = map go $ gm ^. T.ghosts
                 in  gm & T.ghosts  .~ gs
                        & T.status  .~ Running
                        & T.pwrmult .~ 2

wasPowered :: Game -> Game -> Bool
-- ^Determine whether a power pellet was eaten in the last iteration.
wasPowered g0 g1 = t1 == PwrPellet
    where t1 = (g0 ^. T.maze) ! (g1 ^. T.pacman . T.ppos)

wasDepowered :: Game -> Bool
wasDepowered g = case g ^. T.status of
                      PwrRunning _ -> powerTimeLeft g == 0
                      otherwise    -> False

-- =============================================================== --
-- Moving ghosts and player

---------------------------------------------------------------------
-- Moving and updating the player

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
          p1 = moveFrom m0 p0 $ g ^. T.pacman . T.pdir
          t1 = m0 ! p1

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
