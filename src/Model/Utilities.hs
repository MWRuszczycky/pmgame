module Model.Utilities
    ( tickPeriod
    , playerWaitTime
    , ghostWaitTime
    , edibleGhostWaitTime
    , powerDuration
    , isFlashing
    , powerTimeLeft
    , toMicroSeconds
    , newMessage
    , scoreMessage
    , playerScore
    , scoreFruit
    , fruitDuration
    , isWall
    , noWalls
    , moveFrom
    , revDirection
    , pathBetween
    ) where

import qualified Data.Matrix    as M
import qualified Model.Types    as T
import qualified Data.Vector    as V
import Data.Matrix                      ( (!)            )
import Data.List                        ( foldl'         )
import Lens.Micro                       ( (^.)           )
import Model.Types                      ( Tile      (..)
                                        , Score     (..)
                                        , Time      (..)
                                        , Game      (..)
                                        , Maze      (..)
                                        , Point     (..)
                                        , FruitName (..)
                                        , Message   (..)
                                        , Mode      (..)
                                        , Direction (..) )

---------------------------------------------------------------------
-- Default constants

tickPeriod :: Time
-- ^Time between clock ticks.
tickPeriod = toMicroSeconds 0.225

playerWaitTime :: Time
-- ^Wait time for player between moves.
playerWaitTime = tickPeriod

ghostWaitTime :: Time
-- ^Wait time for normal ghosts between moves.
ghostWaitTime = tickPeriod

edibleGhostWaitTime :: Time
-- ^Wait time for edible ghosts between moves.
edibleGhostWaitTime = 2 * ghostWaitTime

messageDuration :: Time
-- ^Length of time messages are displayed.
messageDuration = toMicroSeconds 3

powerDuration :: Time
-- ^Length of time ghosts remain edible after eating a power pellet.
powerDuration = toMicroSeconds 7.5

---------------------------------------------------------------------
-- Useful helper functions

toMicroSeconds :: Double -> Time
-- ^Convert seconds as a Double to microseconds as Time (Int).
toMicroSeconds = round . (* 1000000)

---------------------------------------------------------------------
-- Message helper functions

newMessage :: String -> Message
newMessage s = Message s messageDuration

scoreMessage :: String -> Score -> Message
scoreMessage s score = Message msg messageDuration
    where msg = s ++ " +" ++ show score ++ "!"

---------------------------------------------------------------------
-- Game state query utilities

isFlashing :: Game -> Bool
-- ^General function for querying whether things should be flashing.
isFlashing gm = odd . quot ( gm ^. T.time ) $ tickPeriod

powerTimeLeft :: Game -> Time
-- ^How much power time is left after eating a power pellet.
powerTimeLeft gm = case gm ^. T.mode of
                        PwrRunning t -> t
                        otherwise    -> 0

playerScore :: Game -> Score
-- ^Compute the current score based on the game state.
playerScore gm = pel + ppel + gst + frt
    where pel  = 10 * gm ^. T.items . T.pellets
          ppel = 50 * gm ^. T.items . T.ppellets
          gst  = gm ^. T.items . T.gstscore
          frt  = totalFruitScore $ gm ^. T.items . T.fruits

totalFruitScore :: [(FruitName, Int)] -> Score
totalFruitScore = foldl' ( \ s (fn, n) -> s + n * scoreFruit fn ) 0

---------------------------------------------------------------------
-- Fruit parameters

scoreFruit :: FruitName -> Score
scoreFruit Cherry     = 100
scoreFruit Strawberry = 300
scoreFruit Orange     = 500
scoreFruit Apple      = 700
scoreFruit Melon      = 1000
scoreFruit Galaxian   = 2000
scoreFruit Bell       = 3000
scoreFruit Key        = 5000

fruitDuration :: FruitName -> Time
fruitDuration Cherry     = toMicroSeconds 60
fruitDuration Strawberry = toMicroSeconds 50
fruitDuration Orange     = toMicroSeconds 40
fruitDuration Apple      = toMicroSeconds 30
fruitDuration Melon      = toMicroSeconds 20
fruitDuration Galaxian   = toMicroSeconds 15
fruitDuration Bell       = toMicroSeconds 10
fruitDuration Key        = toMicroSeconds 5

---------------------------------------------------------------------
-- Determining tile subtypes

isWall :: Tile -> Bool
-- ^Evaluate whether a tile is a wall tile.
isWall (Wall _) = True
isWall _        = False

noWalls :: Int -> Int -> V.Vector Tile -> Bool
-- ^Determine if there are no wall tiles between two elements in a
-- vector of tiles.
noWalls x y ts
    | x < y     = not . V.any isWall . V.slice x d $ ts
    | x > y     = not . V.any isWall . V.slice y d $ ts
    | otherwise = False
    where d = abs $ x - y

---------------------------------------------------------------------
-- Utilities for moving player and ghosts

moveFrom :: Maze -> Point -> Direction -> Point
-- ^Get next maze position based on current position and direction.
moveFrom m p d = let go (x0,y0) (x1,y1) = (x0 + x1, y0 + y1)
                     nxt                = go p . dirToShift $ d
                 in  case m ! p of
                          Warp wd wp -> if wd == d then wp else nxt
                          OneWay owd -> if owd == d then nxt else p
                          otherwise  -> nxt

revDirection :: Direction -> Direction
revDirection North = South
revDirection South = North
revDirection West  = East
revDirection East  = West

-- unexported

dirToShift :: Direction -> Point
-- ^Maps directions to single-tick displacements.
dirToShift West  = (0,-1)
dirToShift East  = (0, 1)
dirToShift North = (-1,0)
dirToShift South = (1, 0)

---------------------------------------------------------------------
-- Path-finding

pathBetween :: Maze -> Point -> Point -> [Point]
-- ^Return a list of points that connect p0 and p1 in the maze m.
-- The algorithm works by adding the inital point p0 to the list. All
-- points connected to p0 are then added to the end of the list, and
-- the next point is considered adding all points connected to it to
-- the end of the list that are not already in the list. This repeats
-- until the final point p1 is reached and added as the last element
-- in the list. Thus, the last element of the list (p1) is connected
-- to the first element (p0) via a sequence of points in between.
-- A connected sublist from the last to first is then a path between.
pathBetween m p0 p1
    | p0 == p1  = []
    | otherwise = go . reverse . getPaths m p1 [] $ [p0]
    where go []     = []
          go (x:xs) = go ( dropWhile (not . connected x ) xs ) ++ [x]

-- unexported

getPaths :: Maze -> Point -> [Point] -> [Point] -> [Point]
getPaths m p ys (x:xs)
    | p == x     = ys ++ [x]
    | elem p nxt = ys' ++ [p]
    | otherwise  = getPaths m p ys' (xs ++ nxt)
    where ys' = ys ++ [x]
          nxt = getNxtPoints m (ys ++ xs) x

getNxtPoints :: Maze -> [Point] -> Point -> [Point]
getNxtPoints m xs = filter ( not . flip elem xs ) . go
    where go  (r,c) = filter chk [ (r,c-1), (r,c+1), (r-1,c), (r+1,c) ]
          chk (r,c) = case M.safeGet r c m of
                           Nothing -> False
                           Just t  -> not . isWall $ t

connected :: Point -> Point -> Bool
connected (r1,c1) (r2,c2) = dr + dc < 2
    where dr = abs $ r1 - r2
          dc = abs $ c1 - c2
