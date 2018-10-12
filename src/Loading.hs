{-# LANGUAGE OverloadedStrings #-}
module Loading
    ( initGame
    , levels
    ) where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Model.Types as T
import Lens.Micro                   ( (&), (^.), (.~), (%~) )
import System.Random                ( StdGen, randomR       )
import Model.Utilities              ( messageTime
                                    , toMicroSeconds
                                    , fruitDuration
                                    , powerDuration         )
import Model.Types                  ( Game          (..)
                                    , GameSt        (..)
                                    , Maze          (..)
                                    , Point         (..)
                                    , Ghost         (..)
                                    , GhostName     (..)
                                    , Fruit         (..)
                                    , FruitName     (..)
                                    , GhostState    (..)
                                    , PacMan        (..)
                                    , Tile          (..)
                                    , Time          (..)
                                    , Items         (..)
                                    , Mode          (..)
                                    , Direction     (..)    )

type MazeChars = [(Point, Char)]

---------------------------------------------------------------------
-- List of levels and associated maze files

levels :: [ (Int, FilePath) ]
levels = [ ( -1, "levels/classicMaze1-testing1.txt" )
         , ( 1, "levels/classicMaze1.txt"   ) ]

---------------------------------------------------------------------
-- Game initialization

initGame :: StdGen -> Time -> String -> GameSt
initGame r0 dt s = do
    xs   <- indexMazeString s
    m    <- loadMaze xs
    pman <- loadPacMan xs
    gsts <- mapM ( loadGhost xs ) "pbic"
    (mbFruit, r1) <- loadFruit r0 xs
    return Game { _maze     = m
                , _items    = Items 0 0 0 []
                , _rgen     = r1
                , _pacman   = pman
                , _ghosts   = gsts
                , _fruit    = mbFruit
                , _mode     = Running
                , _level    = 1
                , _npellets = countPellets xs
                , _oneups   = 3
                , _time     = 0
                , _pwrmult  = 2
                , _dtime    = dt
                , _pwrtime  = powerDuration
                , _msg      = Just ("Ready!", messageTime)
                }

---------------------------------------------------------------------
-- Parsing level files

-- Helper functions

getDims :: MazeChars -> (Int, Int)
getDims xs = (maximum rs, maximum cs)
    where (rs,cs) = unzip . fst . unzip $ xs

countPellets :: MazeChars -> Int
countPellets = length . filter isPellet . snd . unzip
    where isPellet x = x == '.' || x == '*'

indexMazeString :: String -> Either String MazeChars
indexMazeString s
    | isRect    = Right . zip indxs . concat $ ss
    | otherwise = Left "Maze is not rectangular"
    where ss     = lines s
          nr     = length ss
          nc     = length . head $ ss
          isRect = all ( (== nc). length ) ss
          indxs  = [ (r,c) | r <- [1..nr], c <- [1..nc] ]

loadPos :: MazeChars -> Char -> Either String Point
loadPos xs x
    | null xs   = Left $ "Cannot find '" ++ [x] ++ "' in maze"
    | otherwise = Right . fst . head $ ys
    where ys = dropWhile ( (/= x) . snd ) xs

-- Reading the player and ghosts

loadPacMan :: MazeChars -> Either String PacMan
loadPacMan xs = do
    pos    <- loadPos xs 'P'
    return PacMan { _pdir   = West
                  , _ppos   = pos
                  , _pstrt  = (pos, West)
                  , _ptlast = 0
                  }

loadGhost :: MazeChars -> Char -> Either String Ghost
loadGhost xs c = do
    pos    <- loadPos xs c
    (n, d) <- readGhost c
    return Ghost { _gname     = n
                 , _gdir      = d
                 , _gpos      = pos
                 , _gstrt     = (pos, d)
                 , _gstate    = Normal
                 , _gtlast    = 0
                 , _gpathback = []
                 }

loadFruit :: StdGen -> MazeChars -> Either String (Maybe Fruit, StdGen)
loadFruit r0 xs = case getFruit r0 xs of
                       Nothing      -> Right (Nothing, r0)
                       Just (f, r1) -> Right (Just f, r1)

readGhost :: Char -> Either String (GhostName, Direction)
readGhost 'p' = Right ( Pinky,  East  )
readGhost 'b' = Right ( Blinky, West  )
readGhost 'i' = Right ( Inky,   West  )
readGhost 'c' = Right ( Clyde,  North )
readGhost x   = Left $ "Character '" ++ [x] ++ "' not recognized as ghost"

-- Reading the maze

loadMaze :: MazeChars -> Either String Maze
loadMaze [] = Left "No maze string provided."
loadMaze xs = M.fromList nr nc <$> mapM (readTile xs) xs
    where (nr,nc) = getDims xs

isWallChar :: Char -> Bool
isWallChar x = x == '|' || x == '='

readTile :: MazeChars -> (Point, Char) -> Either String Tile
readTile _  (_, '.') = Right Pellet
readTile _  (_, '*') = Right PwrPellet
readTile xs (p, 'w') = resolveWarp xs p
readTile _  (_, '^') = Right . OneWay $ North
readTile _  (_, 'v') = Right . OneWay $ South
readTile _  (_, '<') = Right . OneWay $ West
readTile _  (_, '>') = Right . OneWay $ East
readTile xs (p, '|') = Right . resolveWall xs p $ '|'
readTile xs (p, '=') = Right . resolveWall xs p $ '='
readTile _  _        = Right Empty

resolveWarp :: MazeChars -> Point -> Either String Tile
resolveWarp xs (r,c)
    | r == 1    = Warp North <$> wLoc
    | c == 1    = Warp West  <$> wLoc
    | r == nr   = Warp South <$> wLoc
    | c == nc   = Warp East  <$> wLoc
    | otherwise = Left "Incorrect placement of warp tile"
    where (nr, nc) = getDims xs
          wLoc = case filter ( \ (p,x) -> x == 'w' && p /= (r,c) ) xs of
                      []        -> Left "Cannot find matching warp tile"
                      w:[]      -> Right . fst $ w
                      otherwise -> Left "Too many warp tiles"

resolveWall :: MazeChars -> Point -> Char -> Tile
resolveWall xs (r,c) x
    | nw && sw && ww && ew = Wall "╬"
    | nw && sw && ww       = Wall "╣"
    | nw && ww && ew       = Wall "╩"
    | nw && sw && ew       = Wall "╠"
    | sw && ww && ew       = Wall "╦"
    | nw && ww             = Wall "╝"
    | nw && ew             = Wall "╚"
    | sw && ww             = Wall "╗"
    | sw && ew             = Wall "╔"
    | sw || nw             = Wall "║"
    | otherwise            = Wall "═"
    where nw  = verWallLink x . lookup (r-1, c) $ xs
          sw  = verWallLink x . lookup (r+1, c) $ xs
          ww  = horWallLink x . lookup (r, c-1) $ xs
          ew  = horWallLink x . lookup (r, c+1) $ xs

verWallLink :: Char -> Maybe Char -> Bool
verWallLink _   Nothing    = False
verWallLink '|' (Just '|') = True
verWallLink x   (Just y  ) = isWallChar x && isWallChar y && x /= y

horWallLink :: Char -> Maybe Char -> Bool
horWallLink _   Nothing    = False
horWallLink '=' (Just '=') = True
horWallLink x   (Just y  ) = isWallChar x && isWallChar y && x /= y

---------------------------------------------------------------------
-- Fruit

getFruit :: StdGen -> MazeChars -> Maybe (Fruit, StdGen)
getFruit r0 xs = do
    (p,  r1) <- getFruitPosition r0 xs
    (fn, r2) <- getFruitName r1
    let (t0, r3) = getFruitDelay r2
        dt       = fruitDuration fn
    return ( Fruit fn dt t0 p, r3 )

getFruitPosition :: StdGen -> MazeChars -> Maybe (Point, StdGen)
getFruitPosition r0 xs
    | null ps   = Nothing
    | otherwise = Just ( ps !! k, r1 )
    where ps     = [ p | (p, x) <- xs, x == '?' ]
          (k,r1) = randomR (0, length ps - 1) r0

getFruitName :: StdGen -> Maybe (FruitName, StdGen)
getFruitName r0
    | k < 20    = Just ( Cherry,     r1 )
    | k < 40    = Just ( Strawberry, r1 )
    | k < 60    = Just ( Orange,     r1 )
    | k < 80    = Just ( Apple,      r1 )
    | k < 100   = Just ( Melon,      r1 )
    | otherwise = Nothing
    where (k, r1) = randomR (0, 99 :: Int) r0

getFruitDelay :: StdGen -> (Time, StdGen)
getFruitDelay = randomR (tmin, tmax)
    where tmin = toMicroSeconds 0
          tmax = toMicroSeconds 5
