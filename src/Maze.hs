module Maze
    ( initMaze
    , dirToPair
    , isWall
    , isFree
    , sumPair
    , update
    , findTile
    , query
    ) where

import Types        ( St, Maze, Tile (..), Direction (..) )
import Data.List    ( find )

---------------------------------------------------------------------
-- Utilities

sumPair :: Num a => (a, a) -> (a, a) -> (a, a)
sumPair (x0,y0) (x1,y1) = (x0 + x1, y0 + y1)

findTile :: Tile -> Maze -> Maybe (Int, Int)
findTile t = go t . zip [0..]
    where go t []     = Nothing
          go t (r:rs) = case find ((== t) . snd) . zip [0..] . snd $ r of
                             Nothing    -> go t rs
                             Just (c,_) -> Just (fst r, c)

update :: (Int, Int) -> Tile -> Maze -> Maze
update _ _ [] = []
update (x,y) t (r:rs)
    | x > 0     = r : update (x-1,y) t rs
    | x < 0     = r : rs
    | otherwise = updateCol y t r : rs
    where updateCol _ _ [] = []
          updateCol y t (c:cs) | y > 0     = c : updateCol (y-1) t cs
                               | y < 0     = c : cs
                               | otherwise = t : cs

isFree :: (Int, Int) -> Maze -> Bool
isFree (r,c) m = let maxr = length m - 1
                     maxc = (length . head) m - 1
                 in all id [ r >= 0, r <= maxr, c >= 0, c <= maxc
                           , not . isWall . query (r,c) $ m ]

query :: (Int, Int) -> Maze -> Tile
query (r,c) m = m !! r !! c

isWall :: Tile -> Bool
isWall t = elem t [ HBar, VBar, LTee, UTee, RTee, DTee, RDCr, LDCr, RUCr, LUCr ]

dirToPair :: Direction -> (Int, Int)
dirToPair West  = (0,-1)
dirToPair East  = (0, 1)
dirToPair North = (-1,0)
dirToPair South = (1, 0)

---------------------------------------------------------------------
-- Converters from strings

initMaze :: String -> Maze
initMaze s = chop nc . map (convertTile ss) $ ts
    where ts = [ (r,c) | r <- [0 .. nr-1], c <- [0 .. nc-1] ]
          ss = lines s
          nr = length ss
          nc = length . head $ ss

chop :: Int -> [Tile] -> Maze
chop _ [] = []
chop n ts = (:) <$> fst <*> chop n . snd $ splitAt n ts

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
