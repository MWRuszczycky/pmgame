module Resources
    ( -- version information
      version
    -- options declarations
    , optionsHub
    ) where

import qualified Paths_pmgame           as Paths
import qualified System.Console.GetOpt  as O
import qualified Model.Types            as T
import Data.List                                    ( intercalate   )
import Text.Read                                    ( readMaybe     )
import Lens.Micro                                   ( set           )
import Data.Version                                 ( showVersion   )
import Model.Types                                  ( Options (..)  )


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
