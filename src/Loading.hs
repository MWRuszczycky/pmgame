module Loading
    ( initGame
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
-- List of levels and associated maze files

levels :: [ (Int, FilePath) ]
levels = [ ( 1, "data/classicMaze1.txt"  )
         , ( 2, "data/classicMaze1a.txt" ) ]

---------------------------------------------------------------------
-- Game initialization

initGame :: StdGen -> Int -> String -> GameSt
initGame r dt s = do
    xs   <- indexMazeString s
    m    <- loadMaze xs
    pman <- loadPacMan xs
    gsts <- mapM ( loadGhost xs ) "pbic"
    return Game { _maze     = m
                , _items    = Items 0
                , _rgen     = r
                , _pacman   = pman
                , _ghosts   = gsts
                , _status   = Running
                , _level    = 1
                , _npellets = countPellets xs
                , _oneups   = 3
                , _time     = 0
                , _dtime    = dt
                , _pwrtime  = 7500000 }

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
    return PacMan { _pdir  = d
                  , _ppos  = pos
                  , _pstrt = (pos, d)
                  }

loadGhost :: [(Point, Char)] -> Char -> Either String Ghost
loadGhost xs c = do
    pos    <- loadPos xs c
    (t, d) <- initMover c
    return Ghost { _gname = t
                 , _gdir  = d
                 , _gpos  = pos
                 , _gstrt = (pos, d)
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
readTile xs ((r,c), x)
    | isWallChar x = Right . resolveWall xs (r,c) $ x
    | x == '.'     = Right Pellet
    | x == '*'     = Right PwrPellet
    | x == 'w'     = resolveWarp xs (r,c)
    | otherwise    = Right Empty

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
