module Maze
    ( initGame
    , dirToPair
    , isWall
    , isFree
    , sumPair
    ) where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import qualified Types       as T
import Lens.Micro                   ( (&), (^.), (.~), (%~) )
import Data.List                    ( delete
                                    , elemIndex             )
import System.Random                ( StdGen                )
import Types                        ( Game (..)
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
-- Converters from strings

initGame :: StdGen -> String -> Maybe Game
initGame r s = do
    let ss = lines s
        sf = concat ss
    m    <- loadMaze ss
    pman <- loadPacMan sf (M.ncols m)
    gsts <- mapM ( loadGhost sf (M.ncols m) ) "pbic"
    return Game { _maze = m
                , _items = Items 0
                , _rgen = r
                , _pacman = pman
                , _ghosts = gsts
                , _status = Running
                , _remaining = length . filter (== '.') $ sf }

loadMaze :: [String] -> Maybe Maze
loadMaze [] = Nothing
loadMaze ss = Just . M.fromList nr nc . map (convertTile ss) $ ts
    where ts = [ (r,c) | r <- [0 .. nr-1], c <- [0 .. nc-1] ]
          nr = length ss
          nc = length . head $ ss

loadPacMan :: String -> Int -> Maybe PacMan
loadPacMan s nc = do
    pos <- loadPos s nc 'P'
    (_, d) <- initTile 'P'
    return PacMan { _pdir = d, _ppos = pos }

loadGhost :: String -> Int -> Char -> Maybe Ghost
loadGhost s nc c = do
    pos <- loadPos s nc c
    (t, d) <- initTile c
    return Ghost { _gname = t, _gdir = d, _gpos = pos }

loadPos :: String -> Int -> Char -> Maybe (Int, Int)
loadPos s nc c = sumPair (1,1) . flip quotRem nc <$> elemIndex c s

initTile :: Char -> Maybe (Tile, Direction)
initTile 'P' = Just ( Player, North )
initTile 'p' = Just ( Pinky,  North )
initTile 'b' = Just ( Blinky, West  )
initTile 'i' = Just ( Inky,   East  )
initTile 'c' = Just ( Clyde,  South )
initTile _   = Nothing

convertTile :: [String] -> (Int, Int) -> Tile
convertTile ss (x,y)
    | isWallChar c  = resolveWall ss (x,y)
    | c == '.'      = Pellet
    | otherwise     = Empty
    where c = ss !! x !! y

isWallChar :: Char -> Bool
isWallChar c = c == '|' || c == '='

resolveWall :: [String] -> (Int, Int) -> Tile
resolveWall ss (r,c)
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
    where rBnd = subtract 1 . length $ ss
          cBnd = subtract 1 . length . head $ ss
          nw   = r > 0 && isWallChar ( ss !! (r-1) !! c )
          sw   = r < rBnd && isWallChar ( ss !! (r+1) !! c )
          ww   = c > 0 && isWallChar ( ss !! r !! (c-1) )
          ew   = c < cBnd && isWallChar ( ss !! r !! (c+1) )
