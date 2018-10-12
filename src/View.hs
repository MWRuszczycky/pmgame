{-# LANGUAGE OverloadedStrings #-}
module View
    ( drawUI
    , attributes
    ) where

import qualified Graphics.Vty as V
import qualified Data.Text    as Txt
import qualified Data.Matrix  as M
import qualified Model.Types  as T
import Data.List                        ( foldl'                    )
import Lens.Micro                       ( (.~), (^.), (&)           )
import Brick.Types                      ( Widget (..), Padding (..) )
import Brick.Widgets.Core               ( txt, withAttr, vBox, hBox
                                        , str, hLimit, vLimit, fill
                                        , withBorderStyle, padLeft
                                        , (<+>) , emptyWidget       )
import Brick.Widgets.Border.Style       ( unicodeRounded            )
import Brick.Widgets.Border             ( borderWithLabel
                                        , borderAttr                )
import Brick.AttrMap                    ( attrMap, AttrMap          )
import Brick.Util                       ( on, bg, fg                )
import Brick.Widgets.Center             ( center, hCenter, vCenter  )
import Model.Utilities                  ( powerTimeLeft, tickPeriod
                                        , playerScore               )
import Model.Types                      ( Game          (..)
                                        , GameSt        (..)
                                        , PacMan        (..)
                                        , Ghost         (..)
                                        , GhostState    (..)
                                        , GhostName     (..)
                                        , Tile          (..)
                                        , Fruit         (..)
                                        , FruitName     (..)
                                        , Point         (..)
                                        , Direction     (..)
                                        , Status        (..)
                                        , Maze          (..)        )

-- =============================================================== --
-- Drawing the UI for different game states

drawUI :: GameSt -> [ Widget () ]
drawUI (Left msg) = [ str msg ]
drawUI (Right gm) = case gm ^. T.status of
                         GameOver  -> drawGameOverUI gm
                         LevelOver -> drawLevelOverUI gm
                         ReplayLvl -> drawReplayUI gm
                         otherwise -> drawRunningUI gm

drawRunningUI :: Game -> [ Widget () ]
drawRunningUI gm = [ withAttr "background" ui ]
    where ui = center . hLimit ( M.ncols $ gm ^. T.maze ) . vBox $ ws
          ws = [ renderHeader gm
               , renderMaze gm
               , renderOneups gm <+> renderFruit gm
               ]

drawGameOverUI :: Game -> [ Widget () ]
drawGameOverUI gm = [ withAttr "background" msg ]
    where hdr = withAttr "info" . txt $ "GAME OVER!"
          ent = withAttr "info" . txt $ "Enter to play again"
          esc = withAttr "info" . txt $ "Esc to quit"
          msg = center
                . withBorderStyle unicodeRounded
                . borderWithLabel hdr
                . hLimit 25
                . vLimit 3
                . center
                . vBox $ [ renderScore gm, ent, esc ]

drawLevelOverUI :: Game -> [ Widget () ]
drawLevelOverUI gm = [ withAttr "background" msg]
    where hdr = withAttr "info" . txt $ "LEVEL COMPLETED!"
          ent = withAttr "info" . txt $ "Enter to play next level"
          esc = withAttr "info" . txt $ "Esc to quit"
          msg = center
                . withBorderStyle unicodeRounded
                . borderWithLabel hdr
                . hLimit 25
                . vLimit 3
                . center
                . vBox $ [ renderScore gm, ent, esc ]

drawReplayUI :: Game -> [ Widget () ]
drawReplayUI gm = [ withAttr "background" msg]
    where hdr = withAttr "info" . txt $ "YOU GOT CAPTURED!"
          ent = withAttr "info" . txt $ "Enter to keep trying"
          esc = withAttr "info" . txt $ "Esc to quit"
          msg = center
                . withBorderStyle unicodeRounded
                . borderWithLabel hdr
                . hLimit 25
                . vLimit 3
                . center
                . vBox $ [ renderScore gm, ent, esc ]

-- =============================================================== --
-- Tiling functions for constructing the maze prior to rendering

---------------------------------------------------------------------
-- Overall maze construction

tileMaze :: Game -> Maze
tileMaze gm = tilePlayer   ( gm ^. T.pacman )
              . tileGhosts   gm
              . tileFruit  ( gm ^. T.fruit  )
              $ gm ^. T.maze

---------------------------------------------------------------------
-- Tiling the fruit

tileFruit :: Maybe Fruit -> Maze -> Maze
tileFruit Nothing m = m
tileFruit (Just frt) m
    | isWaiting = m
    | otherwise = M.setElem frtTile (frt ^. T.fpos) m
    where isWaiting = frt ^. T.fdelay > 0
          frtTile   = FruitTile $ frt ^. T.fname

---------------------------------------------------------------------
-- Tiling the ghosts

tileGhosts :: Game -> Maze -> Maze
tileGhosts gm m0 = foldl' ( \ m (p,t) -> M.setElem t p m) m0 gs
    where gs = [ (g ^. T.gpos, tileGhost gm g) | g <- gm ^. T.ghosts ]

tileGhost :: Game -> Ghost -> Tile
tileGhost gm g = case g ^. T.gstate of
                      Edible    -> tileEdibleGhost gm g
                      EyesOnly  -> GhostEyes
                      otherwise -> NormalGhost $ g ^. T.gname

tileEdibleGhost :: Game -> Ghost -> Tile
tileEdibleGhost gm g
    | trem >= half = BlueGhost
    | isWhite      = WhiteGhost
    | otherwise    = BlueGhost
    where trem    = powerTimeLeft gm
          half    = quot ( gm ^. T.pwrtime ) 2
          isWhite = odd . quot trem $ tickPeriod

---------------------------------------------------------------------
-- Tiling the player

tilePlayer :: PacMan -> Maze -> Maze
tilePlayer pm = M.setElem Player (pm ^. T.ppos)

-- =============================================================== --
-- Widget rendering

---------------------------------------------------------------------
-- Rendering the maze

renderMaze :: Game -> Widget ()
renderMaze gm = vBox . renderTiles . M.toLists . tileMaze $ gm
    where renderTiles = map ( hBox . map (renderTile gm) )

renderTile :: Game -> Tile -> Widget ()
renderTile _ (Wall s)               = withAttr "maze"       . txt $ s
renderTile _ (OneWay North)         = withAttr "oneway"     . txt $ "-"
renderTile _ (OneWay South)         = withAttr "oneway"     . txt $ "-"
renderTile _ (OneWay West )         = withAttr "oneway"     . txt $ "|"
renderTile _ (OneWay East )         = withAttr "oneway"     . txt $ "|"
renderTile _  Pellet                = withAttr "pellet"     . txt $ "."
renderTile _  PwrPellet             = withAttr "pellet"     . txt $ "*"
renderTile _ (NormalGhost Blinky)   = withAttr "blinky"     . txt $ "\""
renderTile _ (NormalGhost Inky  )   = withAttr "inky"       . txt $ "\""
renderTile _ (NormalGhost Pinky )   = withAttr "pinky"      . txt $ "\""
renderTile _ (NormalGhost Clyde )   = withAttr "clyde"      . txt $ "\""
renderTile _  BlueGhost             = withAttr "blueGhost"  . txt $ "\""
renderTile _  WhiteGhost            = withAttr "whiteGhost" . txt $ "\""
renderTile _  GhostEyes             = withAttr "ghostEyes"  . txt $ "\""
renderTile _ (FruitTile Cherry    ) = withAttr "cherry"     . txt $ "c"
renderTile _ (FruitTile Strawberry) = withAttr "strawberry" . txt $ "s"
renderTile _ (FruitTile Orange    ) = withAttr "orange"     . txt $ "o"
renderTile _ (FruitTile Apple     ) = withAttr "apple"      . txt $ "a"
renderTile _ (FruitTile Melon     ) = withAttr "melon"      . txt $ "m"
renderTile gm Player                = renderPlayer gm
renderTile _  _                     = withAttr "maze"       . txt $ " "

renderPlayer :: Game -> Widget ()
renderPlayer g = withAttr "player" . txt . glyph $ g ^. T.pacman . T.pdir
    where glyph North = "∨"
          glyph South = "∧"
          glyph West  = ">"
          glyph East  = "<"

---------------------------------------------------------------------
-- Rendering score information and messages in header

renderHeader :: Game -> Widget ()
renderHeader gm = vLimit 3 . vBox $ [ fstRow, sndRow, thdRow ]
    where fstRow  = padLeft Max . withAttr "score" . txt $ "High"
          sndRow  = hBox [ renderMessage gm, hsLabel ]
          thdRow  = hBox [ renderScore gm, padLeft Max . renderHighScore $ gm ]
          hsLabel = padLeft Max . withAttr "score" . txt $ "Score"

renderHighScore :: Game -> Widget ()
renderHighScore = renderScore

renderScore :: Game -> Widget ()
renderScore = withAttr "score" . str . show . playerScore

renderMessage :: Game -> Widget ()
renderMessage gm = go $ gm ^. T.msg
    where time            = quot (gm ^. T.time) 1000000
          go Nothing      = withAttr "info" . str . show $ time
          go (Just (s,_)) = withAttr "info" . str $ s

---------------------------------------------------------------------
-- Rendering fruit and oneups

renderOneups :: Game -> Widget ()
renderOneups gm = hBox . take (2 * gm ^. T.oneups) . cycle $ [ oneup, space ]
    where oneup = withAttr "player" . txt $ ">"
          space = withAttr "background" . txt $ " "

renderFruit :: Game -> Widget ()
renderFruit _ = padLeft Max . withAttr "score" . txt $ "fruit!"

-- =============================================================== --
-- Attributes

attributes :: AttrMap
attributes = attrMap V.defAttr
    [ ( "player",     on V.black V.brightYellow  )
    , ( "maze",       on V.blue V.black          )
    , ( "oneway",     on V.red V.black           )
    , ( "pellet",     on V.white V.black         )
    , ( "score",      on V.white V.black         )
    , ( "info",       on V.white V.black         )
    , ( "blinky",     on V.black V.red           )
    , ( "pinky",      on V.black V.brightMagenta )
    , ( "inky",       on V.black V.brightCyan    )
    , ( "clyde",      on V.black V.yellow        )
    , ( "blueGhost",  on V.white V.blue          )
    , ( "whiteGhost", on V.black V.white         )
    , ( "ghostEyes",  on V.cyan V.black          )
    , ( "cherry",     on V.red V.black           )
    , ( "strawberry", on V.red V.black           )
    , ( "orange",     on V.yellow V.black        )
    , ( "apple",      on V.green V.black         )
    , ( "melon",      on V.green V.black         )
    , ( "background", bg V.black                 )
    , ( borderAttr,   on V.blue V.black          )
    ]
