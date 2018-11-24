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
rawMaze 1 = [ "|=========================|"
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
rawMaze 2 = [ "|=========================|"
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
rawMaze 3 = [ "|===========| |===========|"
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
rawMaze 4 = [ "|=========================|"
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
rawMaze 5 = [ "|=========|     |=========|"
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
rawMaze _ = rawMaze 1

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
               , "-- Copying\n"
               , "This is free, open-source software maintained with full"
               , "  documentation and licensing information at:"
               , "  " ++ repository ]

-- =============================================================== --
-- Copying string

copyingHelpStr :: String
copyingHelpStr = intercalate "\n"
  [ "The pmgame binary contains code generated from the following sources. Each of"
  , "the licenses represented are reproduced in full below.\n"
  , "pmgame | BSD-3"
  , "    (c) Mark W. Ruszczycky 2018"
  , "    Author: Mark W. Ruszczycky"
  , "    https://github.com/MWRuszczycky/pmgame\n"
  , "GHC-8.4.3 | GHC-License"
  , "    (c) The University Court of the University of Glasgow 2004"
  , "    https://www.haskell.org/ghc\n"
  , "stack | BSD-3"
  , "    (c) Stack contributors 2015-2018"
  , "    https://docs.haskellstack.org/en/stable/README/"
  , "    https://github.com/commercialhaskell/stack\n"
  , "base-4.11.1.0 | GHC-License, SPJ-Haskell98 & MMTC-Haskell98"
  , "    GHC-License : (c) The University Court of the University of Glasgow 2004"
  , "    SPJ-Haskell98 : (c) Simon Peyton Jones 2002"
  , "    MMTC-Haskell98 : (c) Manuel M. T. Chakravarty 2002"
  , "    https://hackage.haskell.org/package/base\n"
  , "brick-0.37.2 | BSD-3"
  , "    (c) Jonathan Daugherty 2015-2018"
  , "    Author: Jonathan Daugherty"
  , "    Maintainer: Jonathan Daugherty"
  , "    https://hackage.haskell.org/package/brick"
  , "directory-1.3.1.5 | GHC-License & SPJ-Haskell98"
  , "    GHC-License : (c) The University Court of the University of Glasgow 2004"
  , "    SPJ-Haskell98 : (c) Simon Peyton Jones 2002"
  , "    https://hackage.haskell.org/package/directory\n"
  , "matrix-0.3.6.1 | BSD-3"
  , "    (c) Daniel Diaz 2014"
  , "    Author: Daniel Diaz"
  , "    Maintainer: Daniel Diaz"
  , "    https://hackage.haskell.org/package/matrix"
  , "microlens-0.4.9.1 | BSD-3"
  , "    (c) Edward Kmett 2013-2016"
  , "    (c) Artyom Kazak 2015-2016"
  , "    Authors: Edward Kmett, Artyom Kazak"
  , "    Maintainer: Monadfix"
  , "    https://hackage.haskell.org/package/microlens\n"
  , "microlens-th-0.4.2.1 | BSD-3"
  , "    (c) Eric Mertens 2013-2016"
  , "    (c) Edward Kmett 2013-2016"
  , "    (c) Artyom Kazak 2015-2016"
  , "    Authors: Eric Mertens, Edward Kmett, Artyom Kazak"
  , "    Maintainer: Artyom Kazak"
  , "    https://hackage.haskell.org/package/microlens-th\n"
  , "random-1.1 | GHC-License, SPJ-Haskell98 & MMTC-Haskell98"
  , "    GHC-License : (c) The University Court of the University of Glasgow 2004"
  , "    SPJ-Haskell98 : (c) Simon Peyton Jones 2002"
  , "    https://hackage.haskell.org/package/base\n"
  , "    Maintainer: Carter Schonwald"
  , "    https://hackage.haskell.org/package/random"
  , "text-1.2.3.0 | BSD-2"
  , "    (c) Tom Harper 2008-2009"
  , "    (c) Bryan O'Sullivan 2009-2011"
  , "    Author: Bryan O'Sullivan"
  , "    Maintainer: Bryan O'Sullivan"
  , "    https://hackage.haskell.org/package/text\n"
  , "unix-2.7.2.2 | GHC-License"
  , "    (c) The University Court of the University of Glasgow 2004"
  , "    Maintainer: Haskell Libraries Team"
  , "    https://hackage.haskell.org/package/unix\n"
  , "vector-0.12.0.1 | BSD-3"
  , "    (c) Roman Leshchinskiy 2008-2012"
  , "    Author: Roman Leshchinskiy"
  , "    Maintainer: Haskell Libraries Team"
  , "    https://hackage.haskell.org/package/vector\n"
  , "vty-5.21 | BSD-3"
  , "    (c) Stefan O'Rear 2006"
  , "    (c) Corey O'Connor 2008"
  , "    (c) Corey O'Connor 2009"
  , "    Maintainer: Jonathan Daugherty"
  , "    https://hackage.haskell.org/package/vty"
  , "\nDetails of each license type:\n"
  , replicate 80 '-'
  , ghcLicense
  , replicate 80 '-'
  , spj2002
  , replicate 80 '-'
  , mmtc2002
  , replicate 80 '-'
  , bsd2License
  , replicate 80 '-'
  , bsd3License
  ]

bsd2License :: String
bsd2License = unlines
  [ "BSD-2\n"
  , "Redistribution and use in source and binary forms, with or without"
  , "modification, are permitted provided that the following conditions"
  , "are met:\n"
  , "    1. Redistributions of source code must retain the above copyright\n"
  , "       notice, this list of conditions and the following disclaimer.\n"
  , "    2. Redistributions in binary form must reproduce the above"
  , "       copyright notice, this list of conditions and the following"
  , "       disclaimer in the documentation and/or other materials provided"
  , "       with the distribution.\n"
  , "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS"
  , "``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT"
  , "LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR"
  , "A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT"
  , "OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,"
  , "SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT"
  , "LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,"
  , "DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY"
  , "THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT"
  , "(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE"
  , "OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
  ]

bsd3License :: String
bsd3License = unlines
  [ "BSD-3\n"
  , "Redistribution and use in source and binary forms, with or without"
  , "modification, are permitted provided that the following conditions"
  , "are met:\n"
  , "    1. Redistributions of source code must retain the above copyright"
  , "       notice, this list of conditions and the following disclaimer.\n"
  , "    2. Redistributions in binary form must reproduce the above copyright\n"
  , "       notice, this list of conditions and the following disclaimer in the"
  , "       documentation and/or other materials provided with the distribution.\n"
  , "    3. Neither the name of the author nor the names of his contributors"
  , "       may be used to endorse or promote products derived from this software"
  , "       without specific prior written permission.\n"
  , "THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS"
  , "OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED"
  , "WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE"
  , "DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR"
  , "ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL"
  , "DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS"
  , "OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)"
  , "HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,"
  , "STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN"
  , "ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE"
  , "POSSIBILITY OF SUCH DAMAGE."
  ]

ghcLicense :: String
ghcLicense = unlines
  [ "The Glasgow Haskell Compiler License\n"
  , "Copyright 2004, The University Court of the University of Glasgow."
  , "All rights reserved.\n"
  , "Redistribution and use in source and binary forms, with or without"
  , "modification, are permitted provided that the following conditions are met:\n"
  , "    - Redistributions of source code must retain the above copyright notice,"
  , "    this list of conditions and the following disclaimer.\n"
  , "    - Redistributions in binary form must reproduce the above copyright notice,"
  , "    this list of conditions and the following disclaimer in the documentation"
  , "    and/or other materials provided with the distribution.\n"
  , "    - Neither name of the University nor the names of its contributors may be"
  , "    used to endorse or promote products derived from this software without"
  , "    specific prior written permission.\n"
  , "THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF"
  , "GLASGOW AND THE CONTRIBUTORS 'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES,"
  , "INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND"
  , "FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE"
  , "UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE"
  , "FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL"
  , "DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR"
  , "SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER"
  , "CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT"
  , "LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY"
  , "OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH"
  , "DAMAGE."
  ]

spj2002 :: String
spj2002 = unlines
  [ "SPJ-Haskell98\n"
  , "Code derived from the document 'Report on the Programming Language"
  , "Haskell 98', is distributed under the following license:\n"
  , "  Copyright (c) 2002 Simon Peyton Jones\n"
  , "  The authors intend this Report to belong to the entire Haskell"
  , "  community, and so we grant permission to copy and distribute it for"
  , "  any purpose, provided that it is reproduced in its entirety,"
  , "  including this Notice.  Modified versions of this Report may also be"
  , "  copied and distributed for any purpose, provided that the modified"
  , "  version is clearly presented as such, and that it does not claim to"
  , "  be a definition of the Haskell 98 Language."
  ]

mmtc2002 :: String
mmtc2002 = unlines
  [ "MMTC-Haskell98\n"
  , "Code derived from the document 'The Haskell 98 Foreign Function"
  , "Interface, An Addendum to the Haskell 98 Report' is distributed under"
  , "the following license:\n"
  , "  Copyright (c) 2002 Manuel M. T. Chakravarty\n"
  , "  The authors intend this Report to belong to the entire Haskell"
  , "  community, and so we grant permission to copy and distribute it for"
  , "  any purpose, provided that it is reproduced in its entirety,"
  , "  including this Notice.  Modified versions of this Report may also be"
  , "  copied and distributed for any purpose, provided that the modified"
  , "  version is clearly presented as such, and that it does not claim to"
  , "  be a definition of the Haskell 98 Foreign Function Interface."
  ]
