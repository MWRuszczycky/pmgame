module Maze
    ( initMaze
    , dirToPair
    , isWall
    , isFree
    , sumPair
    , findTile
    , randomDirections
    ) where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.List                    ( delete
                                    , nub               )
import System.Random                ( StdGen
                                    , randomR           )
import Types                        ( Game
                                    , Maze
                                    , Ghost (..)
                                    , Tile (..)
                                    , Direction (..)    )

---------------------------------------------------------------------
-- Utilities

sumPair :: Num a => (a, a) -> (a, a) -> (a, a)
sumPair (x0,y0) (x1,y1) = (x0 + x1, y0 + y1)

findTile :: Tile -> Maze -> Maybe (Int, Int)
findTile t m = case V.findIndex (elem t) . M.getMatrixAsVector $ m of
                    Nothing -> Nothing
                    Just k  -> let (r,c) = quotRem k . M.ncols $ m
                               in  Just (r+1, c+1) -- Matrices are 1-indexed

isFree :: Maze -> (Int, Int) -> Bool
isFree m (r,c) = case M.safeGet r c m of
                      Nothing -> False
                      Just t  -> not . isWall $ t

isGhost :: [Tile] -> Bool
isGhost [] = False
isGhost ts = any ( flip elem [ Blinky, Inky, Pinky, Clyde ] ) ts

isWall :: [Tile] -> Bool
isWall []    = False
isWall (t:_) = elem t ws
    where ws = [ HBar, VBar, LTee, UTee, RTee, DTee, RDCr, LDCr, RUCr, LUCr ]

dirToPair :: Direction -> (Int, Int)
dirToPair West  = (0,-1)
dirToPair East  = (0, 1)
dirToPair North = (-1,0)
dirToPair South = (1, 0)

randomDirections :: StdGen -> [Direction] -> (StdGen, [Direction])
randomDirections r0 [] = (r0, [])
randomDirections r0 ds0 = (r, d:ds)
    where (k,r1)  = randomR (0, length ds0 - 1) r0
          d       = ds0 !! k
          ds1     = delete d . nub $ ds0
          (r,ds)  = randomDirections r1 ds1

---------------------------------------------------------------------
-- Converters from strings

initMaze :: String -> Maze
initMaze s = M.fromList nr nc . map (convertTile ss) $ ts
    where ts = [ (r,c) | r <- [0 .. nr-1], c <- [0 .. nc-1] ]
          ss = lines s
          nr = length ss
          nc = length . head $ ss

convertTile :: [String] -> (Int, Int) -> [Tile]
convertTile ss (x,y)
    | isWallChar c  = resolveWall ss (x,y)
    | c == '.'      = [Pellet]
    | c == 'P'      = [Player]
    | c == 'p'      = [Pinky]
    | c == 'i'      = [Inky]
    | c == 'b'      = [Blinky]
    | c == 'c'      = [Clyde]
    | otherwise     = []
    where c = ss !! x !! y

isWallChar :: Char -> Bool
isWallChar c = c == '|' || c == '='

resolveWall :: [String] -> (Int, Int) -> [Tile]
resolveWall ss (r,c)
    | nw && sw && ww && ew = [Cros]
    | nw && sw && ww       = [LTee]
    | nw && ww && ew       = [UTee]
    | nw && sw && ew       = [RTee]
    | sw && ww && ew       = [DTee]
    | nw && ww             = [RDCr]
    | nw && ew             = [LDCr]
    | sw && ww             = [RUCr]
    | sw && ew             = [LUCr]
    | sw || nw             = [VBar]
    | otherwise            = [HBar]
    where rBnd = subtract 1 . length $ ss
          cBnd = subtract 1 . length . head $ ss
          nw   = r > 0 && isWallChar ( ss !! (r-1) !! c )
          sw   = r < rBnd && isWallChar ( ss !! (r+1) !! c )
          ww   = c > 0 && isWallChar ( ss !! r !! (c-1) )
          ew   = c < cBnd && isWallChar ( ss !! r !! (c+1) )
