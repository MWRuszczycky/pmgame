module Maze
    ( initMaze
    , dirToPair
    , isWall
    , isFree
    , sumPair
    , findTile
    ) where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Types                        ( Game, Tile (..), Direction (..) )

---------------------------------------------------------------------
-- Utilities

sumPair :: Num a => (a, a) -> (a, a) -> (a, a)
sumPair (x0,y0) (x1,y1) = (x0 + x1, y0 + y1)

findTile :: Tile -> M.Matrix Tile -> Maybe (Int, Int)
findTile t m = case V.elemIndex t . M.getMatrixAsVector $ m of
                    Nothing -> Nothing
                    Just k  -> let (r,c) = quotRem k . M.ncols $ m
                               in  Just (r+1, c+1) -- Matrices are 1-indexed

isFree :: (Int, Int) -> M.Matrix Tile -> Bool
isFree (r,c) m = case M.safeGet r c m of
                      Nothing -> False
                      Just t  -> not . isWall $ t

isWall :: Tile -> Bool
isWall t = elem t [ HBar, VBar, LTee, UTee, RTee, DTee, RDCr, LDCr, RUCr, LUCr ]

dirToPair :: Direction -> (Int, Int)
dirToPair West  = (0,-1)
dirToPair East  = (0, 1)
dirToPair North = (-1,0)
dirToPair South = (1, 0)

---------------------------------------------------------------------
-- Converters from strings

initMaze :: String -> M.Matrix Tile
initMaze s = M.fromList nr nc . map (convertTile ss) $ ts
    where ts = [ (r,c) | r <- [0 .. nr-1], c <- [0 .. nc-1] ]
          ss = lines s
          nr = length ss
          nc = length . head $ ss

convertTile :: [String] -> (Int, Int) -> Tile
convertTile ss (x,y)
    | isWallChar c  = resolveWall ss (x,y)
    | c == '.'      = Pellet
    | c == 'P'      = Player
    | c == 'p'      = Pinky
    | c == 'i'      = Inky
    | c == 'b'      = Blinky
    | c == 'c'      = Clyde
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
