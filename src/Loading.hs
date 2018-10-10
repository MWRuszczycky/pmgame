{-# LANGUAGE OverloadedStrings #-}
module Loading
    ( initGame
    , levels
    ) where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Model.Types as T
import Lens.Micro                   ( (&), (^.), (.~), (%~) )
import System.Random                ( StdGen                )
import Model.Types                  ( Game          (..)
                                    , GameSt        (..)
                                    , Maze          (..)
                                    , Point         (..)
                                    , Ghost         (..)
                                    , GhostState    (..)
                                    , PacMan        (..)
                                    , Tile          (..)
                                    , Items         (..)
                                    , Status        (..)
                                    , Direction     (..)    )

---------------------------------------------------------------------
-- List of levels and associated maze files

levels :: [ (Int, FilePath) ]
levels = [ ( -1, "levels/classicMaze1-testing1.txt" )
         , ( 1, "levels/classicMaze1.txt"   ) ]

---------------------------------------------------------------------
-- Game initialization

initGame :: StdGen -> Int -> String -> GameSt
initGame r dt s = do
    xs   <- indexMazeString s
    m    <- loadMaze xs
    pman <- loadPacMan xs
    gsts <- mapM ( loadGhost xs ) "pbic"
    return Game { _maze     = m
                , _items    = Items 0 0 0
                , _rgen     = r
                , _pacman   = pman
                , _ghosts   = gsts
                , _status   = Running
                , _level    = 1
                , _npellets = countPellets xs
                , _oneups   = 3
                , _time     = 0
                , _pwrmult  = 2
                , _dtime    = dt
                , _pwrtime  = 7500000
                , _msg      = Just ("Ready!", 1000000)
                }

---------------------------------------------------------------------
-- Parsing level files

-- Helper functions

getDims :: [(Point, Char)] -> (Int, Int)
getDims xs = (maximum rs, maximum cs)
    where (rs,cs) = unzip . fst . unzip $ xs

countPellets :: [(Point, Char)] -> Int
countPellets = length . filter isPellet . snd . unzip
    where isPellet x = x == '.' || x == '*'

indexMazeString :: String -> Either String [(Point, Char)]
indexMazeString s
    | isRect    = Right . zip indxs . concat $ ss
    | otherwise = Left "Maze is not rectangular"
    where ss     = lines s
          nr     = length ss
          nc     = length . head $ ss
          isRect = all ( (== nc). length ) ss
          indxs  = [ (r,c) | r <- [1..nr], c <- [1..nc] ]

loadPos :: [(Point, Char)] -> Char -> Either String Point
loadPos xs x
    | null xs   = Left $ "Cannot find '" ++ [x] ++ "' in maze"
    | otherwise = Right . fst . head $ ys
    where ys = dropWhile ( (/= x) . snd ) xs

-- Reading the player and ghosts

loadPacMan :: [(Point, Char)] -> Either String PacMan
loadPacMan xs = do
    pos    <- loadPos xs 'P'
    (_, d) <- initMover 'P'
    return PacMan { _pdir   = d
                  , _ppos   = pos
                  , _pstrt  = (pos, d)
                  , _ptlast = 0
                  }

loadGhost :: [(Point, Char)] -> Char -> Either String Ghost
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

initMover :: Char -> Either String (Tile, Direction)
initMover 'P' = Right ( Player, West  )
initMover 'p' = Right ( Pinky,  North )
initMover 'b' = Right ( Blinky, West  )
initMover 'i' = Right ( Inky,   East  )
initMover 'c' = Right ( Clyde,  South )
initMover x   = Left $ "Character '" ++ [x] ++ "' not recognized"

-- Reading the maze

loadMaze :: [(Point, Char)] -> Either String Maze
loadMaze [] = Left "No maze string provided."
loadMaze xs = M.fromList nr nc <$> mapM (readTile xs) xs
    where (nr,nc) = getDims xs

isWallChar :: Char -> Bool
isWallChar x = x == '|' || x == '='

readTile :: [(Point, Char)] -> (Point, Char) -> Either String Tile
readTile _  (_, '.') = Right Pellet
readTile _  (_, '*') = Right PwrPellet
readTile xs (p, 'w') = resolveWarp xs p
readTile _  (_, '^') = Right . Door $ North
readTile _  (_, 'v') = Right . Door $ South
readTile _  (_, '<') = Right . Door $ West
readTile _  (_, '>') = Right . Door $ East
readTile xs (p, '|') = Right . resolveWall xs p $ '|'
readTile xs (p, '=') = Right . resolveWall xs p $ '='
readTile _  _        = Right Empty

resolveWarp :: [(Point, Char)] -> Point -> Either String Tile
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

resolveWall :: [(Point, Char)] -> Point -> Char -> Tile
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
