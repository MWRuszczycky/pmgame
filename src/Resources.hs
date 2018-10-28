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
import Model.Utilities                           ( maxMazeCols
                                                 , maxMazeRows
                                                 , minMazeCols
                                                 , minMazeRows      )
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
          ( O.ReqArg ( set T.terminal ) "TERM-ID" )
          terminalUsage
    , O.Option ['m'] ["maze"]
          ( O.ReqArg ( set T.firstmaze . Just ) "MAZE-FILE" )
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
terminalUsage = "Set the TERM variable\n(see below)."

mazeUsage :: String
mazeUsage = "Play your own custom\nmaze as the first\nlevel (see below)."

backdoorUsage :: String
backdoorUsage = "Jump to a specific level for testing purposes."

repository :: String
repository = "https://github.com/MWRuszczycky/pmgame.git"

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
    where isFlipped = odd . quot (lvl - 1) $ 10
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
        , "    |.|====| | |====|.|===|"
        , "    |........|........|____"
        , "|===|.|====|.|.|====|.|===|"
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
        , "|.|==|...... | ......|==|.|"
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

-- =============================================================== --
-- General help string

helpStr :: String
helpStr = intercalate "\n" hs
    where spacer = "----------------------------------------------------------"
          hs = [ "Usage: pmgame [OPTION]...\n"
               , "Play a Pac-Man-like game in your terminal!"
               , "pmgame is written entirely in Haskell using Brick.\n"
               , O.usageInfo (spacer ++ "\n-- Available options\n") . init
                    $ optionsHub
               , spacer
               , "-- Terminal and colors help\n"
               , "The default TERM parameter used by pmgame is xterm-256color."
               , "If this prevents the game from running in your terminal,"
               , "then you can use the -t/--terminal option to reset it, e.g.,"
               , "\n    pmgame --terminal=xterm-16color\n\nor"
               , "\n    pmgame --terminal=xterm\n"
               , "The game requires at least 16 colors. So if you choose a"
               , "TERM parameter that does not support 16 colors, the game"
               , "should still run, but some colors may not show up correctly."
               , "\n" ++ spacer
               , "-- Creating your own mazes\n"
               , "You can create your own maze to play as the first level by"
               , "providing a path to an ascii-maze file using the -m/--maze"
               , "option. After completing this maze, the game will continue"
               , "on as normal.\n"
               , "Tiles are designated by the following ascii characters:\n"
               , "    = : wall with horizontal hint"
               , "    | : wall with vertical hint"
               , "    P : player start position"
               , "    p : pinky (pink ghost) start position"
               , "    i : inky (cyan ghost) start position"
               , "    b : blinky (red ghost) start position"
               , "    c : clyde (yellow ghost) start position"
               , "    . : regular pellet"
               , "    * : power pellet"
               , "    w : warp"
               , "    < : west-oneway"
               , "    > : east-oneway"
               , "    ^ : north-oneway"
               , "    v : south-oneway"
               , "    ? : fruit position\n"
               , "Rules for constructing an ascii maze:\n"
               , "  1. Mazes must be rectangular and have at least "
                 ++ show minMazeRows ++ " rows"
               , "     and " ++ show minMazeCols
                 ++ " columns and no more than " ++ show maxMazeRows ++ " rows "
                 ++ "and " ++ show maxMazeCols ++ " columns."
               , "     Note that you can produce non-rectangular maze shapes by"
               , "     using underscores and spaces to fill out the necessary"
               , "     number of rows and colums (see example below).\n"
               , "  2. The maze must be fully enclosed so that neither the"
               , "     player nor the ghosts can escape.\n"
               , "  3. Wall hints (i.e., '=' vs. '|') are used to help produce"
               , "     connections and corners correctly (see example below).\n"
               , "  4. There must be at least one 'P' (player) character. If"
               , "     there is more than one 'P' character, then only the"
               , "     first will be used.\n"
               , "  5. All four of the ghosts must be present. If any ghost is"
               , "     repeated, then only the first will be used.\n"
               , "  6. There must be at least one pellet (regular or power).\n"
               , "  7. There can be either no warps or two warps. Any other"
               , "     number of warps will be rejected. The warps must also"
               , "     be on the outermost edge of the ascii maze.\n"
               , "  8. Oneways can only be entered and exited in a single"
               , "     direction. These are usually used to keep ghosts from"
               , "     returning to their starting box. Oneways can be placed"
               , "     anywhere; however, if a ghost or the player can escape"
               , "     by replacing the oneway with an empty space, then the"
               , "     maze will be rejected.\n"
               , "  9. Fruit appears randomly at a random position. Only"
               , "     cherries can appear in the first level. Use the '?'"
               , "     character to indicate a position where a cherry can"
               , "     appear. If you have no '?' characters, then a cherry"
               , "     will never appear.\n"
               , " 10. Any unrecognized character including a blank space is"
               , "     interpreted as an empty space. In the example below,"
               , "     underscores are used to make sure that the maze is"
               , "     rectangular without having trailing whitespace.\n"
               , "Below is a small example ascii maze that will work:\n"
               , "                  |=============|"
               , "                  |*.. ????? ..*|"
               , "                  |...|=^^^=|...|"
               , "                  |...| ipbc|...|"
               , "                  |=|.|=====|.|=|"
               , "                    |. ??|?? .|__"
               , "                  |=|....|....|=|"
               , "                  w  .|=====|.  w"
               , "                  |=|.........|=|"
               , "                  |...|=====|...|"
               , "                  |*.....P.....*|"
               , "                  |=============|"
               , "\n" ++ spacer
               , "-- Copyright Mark W. Ruszczycky (c) 2018\n"
               , "This is open-source software that you are free to change and"
               , "     redistribute according to the BSD-3-Clause license,"
               , "     https://opensource.org/licenses/BSD-3-Clause"
               , "The open-source repository is maintained at: "
               , "     " ++ repository ]
