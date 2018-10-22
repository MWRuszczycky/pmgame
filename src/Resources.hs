module Resources
    ( -- version information
      version
    -- options declarations
    , optionsHub
    -- Maze numbers and ascii mazes
    , getAsciiMaze
    , mazeNumber
    ) where

import qualified Paths_pmgame           as Paths
import qualified System.Console.GetOpt  as O
import qualified Model.Types            as T
import Data.List                                 ( intercalate      )
import Text.Read                                 ( readMaybe        )
import Lens.Micro                                ( set              )
import Data.Version                              ( showVersion      )
import Model.Types                               ( Options   (..)
                                                 , AsciiMaze (..)   )


---------------------------------------------------------------------
---------------------------------------------------------------------
-- Resources for the game such as help strings and maze strings.

-- =============================================================== --
-- Version information

version :: String
version = "pmgame version " ++ showVersion Paths.version

-- =============================================================== --
-- Declaration of command line arguments, options and usage info

-- Exported

optionsHub :: [ O.OptDescr (Options -> Options) ]
-- ^Used by GetOpt.getOpt to parse command line options. Contains
-- handlers for all possible command line options.
optionsHub =
    [ O.Option ['h'] ["help"]
          ( O.NoArg ( set T.info (Just helpStr) ) )
          helpUsage
    , O.Option ['v'] ["version"]
          ( O.NoArg ( set T.info (Just version) ) )
          versionUsage
    , O.Option ['t'] ["terminal"]
          ( O.ReqArg ( set T.terminal ) "TERMINAL-IDENTIFIER" )
          terminalUsage
    , O.Option ['m'] ["maze"]
          ( O.ReqArg ( set T.firstmaze . Just ) "PATH-TO-MAZE-FILE" )
          mazeUsage
      -- backdoor should be the last option, because it should not be
      -- displayed in the help string.
    , O.Option ['b'] ["backdoor"]
          ( O.ReqArg ( set T.firstlevel . readFirstLevel ) "LEVEL-NUMBER" )
          backdoorUsage
    ]

-- Unexported

readFirstLevel :: String -> Int
-- ^Read a string as level value, and if not possible, then just
-- default to level 1.
readFirstLevel = maybe 1 id . readMaybe

helpUsage :: String
helpUsage = "Show this help display."

versionUsage :: String
versionUsage = "Show version number."

terminalUsage :: String
terminalUsage = "Set the TERM variable (see below)."

mazeUsage :: String
mazeUsage = "Maze file to play as the first level."

backdoorUsage :: String
backdoorUsage = "Jump to a specific level for testing purposes."

---------------------------------------------------------------------
-- General help string

repository :: String
repository = "https://github.com/MWRuszczycky/pmgame.git"

helpStr :: String
helpStr = intercalate "\n" hs
    where hs = [ "Usage: pmgame [OPTION]...\n"
               , "Play a Pac-Man-like game in your terminal!"
               , "pmgame is written entirely in Haskell using Brick.\n"
               , O.usageInfo "Available options:" . init $ optionsHub
               , "Terminal and colors help:"
               , "The default TERM parameter used by pmgame is xterm-256color."
               , "If this prevents the game from running in your terminal,"
               , "then you can use the -t/--terminal option to reset it, e.g.,"
               , "    pmgame --terminal=xterm-16color\nor"
               , "    pmgame --terminal=xterm"
               , "The game requires at least 16 colors. So if you choose a"
               , "TERM parameter that does not support 16 colors, the game"
               , "should still run, but some colors may not show up correctly."
               , "\nRepository: " ++ repository
               , "License: BSD3" ]

-- =============================================================== --
-- Maze numbering system and ascii maze strings

-- Exported

mazeNumber :: Int -> Int
-- ^Maps level numbers to maze numbers.
mazeNumber n
    | n > 0     = rem (m - 1) 5 + 1
    | otherwise = n
    where m = quot ( rem n 2 + n ) 2

getAsciiMaze :: Int -> AsciiMaze
-- ^Return an ascii maze based on the level number.
getAsciiMaze lvl
    | lvl < 1   = getAsciiMaze 1
    | isFlipped = intercalate "\n" . flipMaze $ maze
    | otherwise = intercalate "\n" $ maze
    where isFlipped = odd . quot lvl $ 10
          maze      = rawMaze . mazeNumber $ lvl

flipMaze :: [String] -> [String]
-- ^Flip a maze upside-down and invert all North/South oneways.
flipMaze = map ( map go ) . reverse
    where go x | x == '^'  = 'v'
               | x == 'v'  = '^'
               | otherwise = x

-- Unexported

rawMaze :: Int -> [String]
rawMaze 1 = maze1
rawMaze 2 = maze2
rawMaze 3 = maze3
rawMaze 4 = maze4
rawMaze 5 = maze5
rawMaze _ = maze1

maze1 :: [String]
maze1 = [ "|=========================|"
        , "|............|............|"
        , "|*|==|.|===|.|.|===|.|==|*|"
        , "|.|==|.|===|.|.|===|.|==|.|"
        , "|.........................|"
        , "|.|==|.|.|=======|.|.|==|.|"
        , "|.|==|.|.|=======|.|.|==|.|"
        , "|......|.....|.....|......|"
        , "|====|.|==== | ====|.|====|"
        , "     |.|???????????|.|_____"
        , "|====|.|?|==^^^==|?|.|====|"
        , "w     . ?| bi pc |? ._____w"
        , "|====|.|?|=======|?|.|====|"
        , "     |.|???????????|.|_____"
        , "|====|.| |=======| |.|====|"
        , "|............|............|"
        , "|.|==|.|===|.|.|===|.|==|.|"
        , "|*...|.......P.......|...*|"
        , "|==|.|.|.|===|===|.|.|.|==|"
        , "|......|.....|.....|......|"
        , "|.|========|.|.|========|.|"
        , "|.........................|"
        , "|=========================|"
        ]

maze2 :: [String]
maze2 = [ "|=========================|"
        , "|*......|.........|......*|"
        , "|.|===|.|.|=====|.|.|===|.|"
        , "|.........................|"
        , "|===|.|.|=|.|=|.|=|.|.|===|"
        , "w    .|.|=|.| |.|=|.|.|____"
        , "|===|.|.....| |.....|.|____"
        , "    |.|===| |=| |===|.|____"
        , "    |. ????????????? .|____"
        , "    |.|=|?|=^^^=|?|=|.|____"
        , "    |.|???| ibcp|???|.|____"
        , "    |.|?|?|=====|?|?|.|===|"
        , "    |. ?|?????????|? .    w"
        , "    |.|====|.|.|====|.|===|"
        , "    |......|.|.|......|____"
        , "|===|.|==|.|.|.|.|==|.|===|"
        , "|............|............|"
        , "|.|===|.|==|.|.|==|.|===|.|"
        , "|.....|.|....P....|.|.....|"
        , "|.|=|.|...|=====|...|.|=|.|"
        , "|*|=|.|=|.|=====|.|=|.|=|*|"
        , "|.........................|"
        , "|=========================|"
        ]

maze3 :: [String]
maze3 = [ "|===========| |===========|"
        , "w     ......| |......     w"
        , "|====|.|==|.|=|.|==|.|====|"
        , "|*.....|==|.....|==|.....*|"
        , "|.|==|......|=|......|==|.|"
        , "|.|==|.|==|.| |.|==|.|==|.|"
        , "|......|==|.|=|.|==|......|"
        , "|.|==|.......|.......|==|.|"
        , "|.|  |.|===| | |===|.|  |.|"
        , "|.|==|.|???????????|.|==|.|"
        , "|...... ?|==^^^==|? ......|"
        , "|.|==|.|?| ip cb |?|.|==|.|"
        , "|.|==|.|?|=======|?|.|==|.|"
        , "|...... ??????????? ......|"
        , "|==|.|=| |=| | |=| |=|.|==|"
        , "   |.|=| |=| | |=| |=|.|___"
        , "   |.........P.........|___"
        , "   |.|=|.|=|.|.|=|.|=|.|___"
        , "|==|.|=|.|...|...|.|=|.|==|"
        , "|......|.|.|===|.|.|......|"
        , "|.|==|.|.|.|===|.|.|.|==|.|"
        , "|*.......................*|"
        , "|=========================|"
        ]

maze4 :: [String]
maze4 = [ "|=========================|"
        , "|.........................|"
        , "|*|.|==|.|=======|.|==|.|*|"
        , "|.|......|.......|......|.|"
        , "|.|==|.|.|.|===|.|.|.|==|.|"
        , "|......|.....|.....|......|"
        , "|====|.|===|.|.|===|.|====|"
        , "w     .......|.......     w"
        , "|====|.| |=| | |=| |.|====|"
        , "|......|???????????|......|"
        , "|.|==|.|?|==^^^==|?|.|==|.|"
        , "|.|==|. ?| ci bp |? .|==|.|"
        , "|......|?|=======|?|......|"
        , "|==|.|.|???????????|.|.|==|"
        , "   |.|.| |=======| |.|.|___"
        , "   |.|...|.......|...|.|___"
        , "   |.|=|.|.|===|.|.|=|.|___"
        , "   |.........P.........|___"
        , "|==|.|=====|.|.|=====|.|==|"
        , "|....|.......|.......|....|"
        , "|.|==|.|===========|.|==|.|"
        , "|*.......................*|"
        , "|=========================|"
        ]

maze5 :: [String]
maze5 = [ "|=========|     |=========|"
        , "|*........|     |........*|"
        , "|.|=====|.|=====|.|=====|.|"
        , "|.|.....................|.|"
        , "|...|==|.|=|.|.|=|.|==|...|"
        , "|.|.|  |.|=|.|.|=|.|  |.|.|"
        , "|.|.|  |.....|.....|  |.|.|"
        , "|.|.|==| |=======| |==|.|.|"
        , "|.|.... ??????????? ....|.|"
        , "|.|==|.|?|=======|?|.|==|.|"
        , "|......|?| ci pb |?|......|"
        , "|.|.|==|?|==vvv==|?|==|.|.|"
        , "|.|.... ??????????? ....|.|"
        , "|.|==|.|=| |===| |=|.|==|.|"
        , "|......|=| |===| |=|......|"
        , "|==| |.......|.......| |==|"
        , "w    |.|.|=|.|.|=|.|.|    w"
        , "|====|.|.| |.|.| |.|.|====|"
        , "|......|.|=|.|.|=|.|......|"
        , "|.|==|.|.....P.....|.|==|.|"
        , "|*|==|.|.|=======|.|.|==|*|"
        , "|.........................|"
        , "|=========================|"
        ]
