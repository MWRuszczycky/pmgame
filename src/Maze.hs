module Maze
    ( initGame
    , dirToPair
    , isWall
    , isFree
    , sumPair
    , levels
    ) where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Types       as T
import Lens.Micro                   ( (&), (^.), (.~), (%~) )
import System.Random                ( StdGen                )
import Types                        ( Game (..)
                                    , GameSt (..)
                                    , Maze
                                    , Point (..)
                                    , Ghost (..)
                                    , PacMan (..)
                                    , Tile (..)
                                    , Items (..)
                                    , Status (..)
                                    , Direction (..)        )

---------------------------------------------------------------------
-- Utilities

sumPair :: Num a => (a, a) -> (a, a) -> (a, a)
sumPair (x0,y0) (x1,y1) = (x0 + x1, y0 + y1)

isFree :: Maze -> Point -> Bool
isFree m (r,c) = case M.safeGet r c m of
                      Nothing -> False
                      Just t  -> not . isWall $ t

isWall :: Tile -> Bool
isWall t = elem t ws
    where ws = [ HBar, VBar, LTee, UTee, RTee, DTee, RDCr, LDCr, RUCr, LUCr ]

dirToPair :: Direction -> Point
dirToPair West  = (0,-1)
dirToPair East  = (0, 1)
dirToPair North = (-1,0)
dirToPair South = (1, 0)

---------------------------------------------------------------------
-- Game initialization

initGame :: StdGen -> String -> GameSt
initGame r s = do
    let sf = indexMazeString s
    m    <- loadMaze sf
    pman <- loadPacMan sf
    gsts <- mapM ( loadGhost sf ) "pbic"
    return Game { _maze     = m
                , _items    = Items 0
                , _rgen     = r
                , _pacman   = pman
                , _ghosts   = gsts
                , _status   = Running
                , _level    = 1
                , _npellets = countPellets sf
                , _oneups   = 3 }

indexMazeString :: String -> [(Point, Char)]
indexMazeString s = zip [ (r,c) | r <- [1..nr], c <- [1..nc] ] (concat ss)
    where ss = lines s
          nr = length ss
          nc = length . head $ ss

getDims :: [(Point, Char)] -> (Int, Int)
getDims sf = (maximum rs, maximum cs)
    where (rs,cs) = unzip . fst . unzip $ sf

isWallChar :: Char -> Bool
isWallChar x = x == '|' || x == '='

countPellets :: [(Point, Char)] -> Int
countPellets = length . filter (== '.') . snd . unzip

loadMaze :: [(Point, Char)] -> Either String Maze
loadMaze [] = Left "No maze string provided."
loadMaze sf = M.fromList nr nc <$> mapM (readTile sf) sf
    where (nr,nc) = getDims sf

loadPacMan :: [(Point, Char)] -> Either String PacMan
loadPacMan sf = do
    pos    <- loadPos sf 'P'
    (_, d) <- initMover 'P'
    return PacMan { _pdir  = d
                  , _ppos  = pos
                  , _pstrt = (pos, d)
                  }

loadGhost :: [(Point, Char)] -> Char -> Either String Ghost
loadGhost sf c = do
    pos    <- loadPos sf c
    (t, d) <- initMover c
    return Ghost { _gname = t
                 , _gdir  = d
                 , _gpos  = pos
                 , _gstrt = (pos, d)
                 }

loadPos :: [(Point, Char)] -> Char -> Either String Point
loadPos sf x
    | null xs   = Left $ "Cannot find '" ++ [x] ++ "' in maze"
    | otherwise = Right . fst . head $ xs
    where xs = dropWhile ( (/= x) . snd ) sf

initMover :: Char -> Either String (Tile, Direction)
initMover 'P' = Right ( Player, West  )
initMover 'p' = Right ( Pinky,  North )
initMover 'b' = Right ( Blinky, West  )
initMover 'i' = Right ( Inky,   East  )
initMover 'c' = Right ( Clyde,  South )
initMover x   = Left $ "Character '" ++ [x] ++ "' not recognized"

readTile :: [(Point, Char)] -> (Point, Char) -> Either String Tile
readTile sf ((r,c), x)
    | isWallChar x = Right . resolveWall sf $ (r,c)
    | x == '.'     = Right Pellet
    | x == 'w'     = resolveWarp sf (r,c)
    | otherwise    = Right Empty

resolveWarp :: [(Point, Char)] -> Point -> Either String Tile
resolveWarp sf (r,c)
    | r == 1    = Warp North <$> wLoc
    | c == 1    = Warp West  <$> wLoc
    | r == nr   = Warp South <$> wLoc
    | c == nc   = Warp East  <$> wLoc
    | otherwise = Left "Incorrect placement of warp tile"
    where (nr, nc) = getDims sf
          wLoc = case filter ( \ (p,x) -> x == 'w' && p /= (r,c) ) sf of
                      []        -> Left "Cannot find matching warp tile"
                      w:[]      -> Right . fst $ w
                      otherwise -> Left "Too many warp tiles"

resolveWall :: [(Point, Char)] -> Point -> Tile
resolveWall sf (r,c)
    | nw && sw && ww && ew = Cros
    | nw && sw && ww       = LTee
    | nw && ww && ew       = UTee
    | nw && sw && ew       = RTee
    | sw && ww && ew       = DTee
    | nw && ww             = RDCr
    | nw && ew             = LDCr
    | sw && ww             = RUCr
    | sw && ew             = LUCr
    | sw || nw             = VBar
    | otherwise            = HBar
    where nw  = chk . lookup (r-1, c) $ sf
          sw  = chk . lookup (r+1, c) $ sf
          ww  = chk . lookup (r, c-1) $ sf
          ew  = chk . lookup (r, c+1) $ sf
          chk Nothing  = False
          chk (Just x) = isWallChar x

---------------------------------------------------------------------
-- Levels

levels :: [ (Int, FilePath) ]
levels = [ ( 1, "data/classicMaze1.txt"  )
         , ( 2, "data/classicMaze1a.txt" ) ]
