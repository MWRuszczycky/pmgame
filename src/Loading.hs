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
                                    , powerDuration         )
import Model.Types                  ( Game          (..)
                                    , GameSt        (..)
                                    , Maze          (..)
                                    , Point         (..)
                                    , Ghost         (..)
                                    , Fruit         (..)
                                    , GhostState    (..)
                                    , PacMan        (..)
                                    , Tile          (..)
                                    , Items         (..)
                                    , Status        (..)
                                    , Direction     (..)    )

type MazeChars = [(Point, Char)]

---------------------------------------------------------------------
-- List of levels and associated maze files

levels :: [ (Int, FilePath) ]
levels = [ ( -1, "levels/classicMaze1-testing1.txt" )
         , ( 1, "levels/classicMaze1.txt"   ) ]

---------------------------------------------------------------------
-- Game initialization

initGame :: StdGen -> Int -> String -> GameSt
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
                , _status   = Running
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
    (_, d) <- initMover 'P'
    return PacMan { _pdir   = d
                  , _ppos   = pos
                  , _pstrt  = (pos, d)
                  , _ptlast = 0
                  }

loadGhost :: MazeChars -> Char -> Either String Ghost
loadGhost xs c = do
    pos    <- loadPos xs c
    (t, d) <- initMover c
    return Ghost { _gname     = t
                 , _gdir      = d
                 , _gpos      = pos
                 , _gstrt     = (pos, d)
                 , _gstate    = Normal
                 , _gtlast    = 0
                 , _gpathback = []
                 }

loadFruit :: StdGen -> MazeChars -> Either String (Maybe Fruit, StdGen)
loadFruit r0 xs = case getFruitPosition r0 xs of
                       (Nothing, r1) -> Right (Nothing, r1)
                       (Just p, r1 ) -> Right . getFruit r1 $ p

initMover :: Char -> Either String (Tile, Direction)
initMover 'P' = Right ( Player, West  )
initMover 'p' = Right ( Pinky,  East  )
initMover 'b' = Right ( Blinky, West  )
initMover 'i' = Right ( Inky,   West  )
initMover 'c' = Right ( Clyde,  North )
initMover x   = Left $ "Character '" ++ [x] ++ "' not recognized"

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

getFruitPosition :: StdGen -> MazeChars -> (Maybe Point, StdGen)
getFruitPosition r0 xs
    | null ps   = ( Nothing, r0 )
    | otherwise = ( Just $ ps !! k, r1 )
    where ps     = [ p | (p, x) <- xs, x == '?' ]
          (k,r1) = randomR (0, length ps - 1) r0

getFruit :: StdGen -> Point -> (Maybe Fruit, StdGen)
getFruit r0 p = ( pickFruit, r1 )
    where (k, r1)   = randomR (0, 99 :: Int) r0
          pickFruit | k < 20  = Just . cherry $ p
                    | k < 40  = Just . strawberry $ p
                    | k < 60  = Just . orange $ p
                    | k < 80  = Just . apple $ p
                    | k < 100 = Just . melon $ p
                    | otherwise = Nothing

cherry :: Point -> Fruit
cherry p = Fruit
   { _fname     = Cherry
   , _fduration = 6000000
   , _fdelay    = 600000
   , _fpos      = p
   , _fpoints   = 100
   }

strawberry :: Point -> Fruit
strawberry p = Fruit
    { _fname     = Strawberry
    , _fduration = 5500000
    , _fdelay    = 550000
    , _fpos      = p
    , _fpoints   = 300
    }

orange :: Point -> Fruit
orange p = Fruit
    { _fname     = Orange
    , _fduration = 5000000
    , _fdelay    = 500000
    , _fpos      = p
    , _fpoints   = 500
    }

apple :: Point -> Fruit
apple p = Fruit
    { _fname     = Orange
    , _fduration = 5500000
    , _fdelay    = 450000
    , _fpos      = p
    , _fpoints   = 700
    }

melon :: Point -> Fruit
melon p = Fruit
    { _fname     = Melon
    , _fduration = 6000000
    , _fdelay    = 400000
    , _fpos      = p
    , _fpoints   = 1000
    }
