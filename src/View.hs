{-# LANGUAGE OverloadedStrings #-}
module View
    ( drawUI
    , attributes
    ) where

import qualified Graphics.Vty as V
import qualified Data.Text    as Txt
import qualified Data.Matrix  as M
import qualified Types        as T
import Data.List                        ( foldl'                    )
import Lens.Micro                       ( (.~), (^.), (&)           )
import Brick.Types                      ( Widget (..), Padding (..) )
import Brick.Widgets.Core               ( txt, withAttr, vBox, hBox
                                        , str, hLimit, vLimit, fill
                                        , withBorderStyle, padLeft
                                        , (<+>)                     )
import Brick.Widgets.Border.Style       ( unicodeRounded            )
import Brick.Widgets.Border             ( borderWithLabel
                                        , borderAttr                )
import Brick.AttrMap                    ( attrMap, AttrMap          )
import Brick.Util                       ( on, bg, fg                )
import Brick.Widgets.Center             ( center, hCenter, vCenter  )
import Types                            ( Game (..)
                                        , GameSt (..)
                                        , Tile (..)
                                        , Direction (..)
                                        , Status (..)
                                        , Maze (..)                 )

drawUI :: GameSt -> [ Widget () ]
drawUI (Left msg) = [ str msg ]
drawUI (Right g)  = case g ^. T.status of
                        Running   -> drawRunningUI g
                        GameOver  -> drawGameOverUI g
                        LevelOver -> drawLevelOverUI g
                        ReplayLvl -> drawReplayUI g

drawRunningUI :: Game -> [ Widget () ]
drawRunningUI g = [ withAttr "background" ui ]
    where ui = center . hLimit ( M.ncols $ g ^. T.maze ) . vBox $ ws
          ws = [ renderScore g
               , renderMaze g
               , renderOneups g <+> renderFruit g
               ]

drawGameOverUI :: Game -> [ Widget () ]
drawGameOverUI g = [ withAttr "background" msg ]
    where hdr = withAttr "score" . txt $ "GAME OVER!"
          ent = withAttr "score" . txt $ "Enter to play again"
          esc = withAttr "score" . txt $ "Esc to quit"
          msg = center
                . withBorderStyle unicodeRounded
                . borderWithLabel hdr
                . hLimit 25
                . vLimit 3
                . center
                . vBox $ [ renderScore g, ent, esc ]

drawLevelOverUI :: Game -> [ Widget () ]
drawLevelOverUI g = [ withAttr "background" msg]
    where hdr = withAttr "score" . txt $ "LEVEL COMPLETED!"
          ent = withAttr "score" . txt $ "Enter to play next level"
          esc = withAttr "score" . txt $ "Esc to quit"
          msg = center
                . withBorderStyle unicodeRounded
                . borderWithLabel hdr
                . hLimit 25
                . vLimit 3
                . center
                . vBox $ [ renderScore g, ent, esc ]

drawReplayUI :: Game -> [ Widget () ]
drawReplayUI g = [ withAttr "background" msg]
    where hdr = withAttr "score" . txt $ "YOU GOT CAPTURED!"
          ent = withAttr "score" . txt $ "Enter to keep trying"
          esc = withAttr "score" . txt $ "Esc to quit"
          msg = center
                . withBorderStyle unicodeRounded
                . borderWithLabel hdr
                . hLimit 25
                . vLimit 3
                . center
                . vBox $ [ renderScore g, ent, esc ]

---------------------------------------------------------------------
-- Widget rendering

renderScore :: Game -> Widget ()
renderScore g = withAttr "score" . str . show $ s
    where s = 10 * g ^. T.items . T.pellets

renderOneups :: Game -> Widget ()
renderOneups g = hBox . take (2 * g ^. T.oneups) . cycle $ [ oneup, space ]
    where oneup = withAttr "player" . txt . playerGlyph $ West
          space = withAttr "background" . txt $ " "

renderFruit :: Game -> Widget ()
renderFruit g = padLeft Max . withAttr "score" . txt $ "fruit!"

renderMaze :: Game -> Widget ()
renderMaze g = vBox . map ( hBox . map (renderTile g) ) . M.toLists $ m2
    where gs = g ^. T.ghosts
          m0 = g ^. T.maze
          m1 = M.setElem Player ( g ^. T.pacman . T.ppos ) m0
          m2 = foldl' (\ m x -> M.setElem (x ^. T.gname) (x ^. T.gpos) m) m1 gs

renderTile :: Game -> Tile -> Widget ()
renderTile _ Empty      = withAttr "maze"   . txt $ " "
renderTile _ HBar       = withAttr "maze"   . txt $ "═"
renderTile _ VBar       = withAttr "maze"   . txt $ "║"
renderTile _ Cros       = withAttr "maze"   . txt $ "╬"
renderTile _ LTee       = withAttr "maze"   . txt $ "╣"
renderTile _ RTee       = withAttr "maze"   . txt $ "╠"
renderTile _ DTee       = withAttr "maze"   . txt $ "╦"
renderTile _ UTee       = withAttr "maze"   . txt $ "╩"
renderTile _ LUCr       = withAttr "maze"   . txt $ "╔"
renderTile _ RUCr       = withAttr "maze"   . txt $ "╗"
renderTile _ LDCr       = withAttr "maze"   . txt $ "╚"
renderTile _ RDCr       = withAttr "maze"   . txt $ "╝"
renderTile _ Pellet     = withAttr "pellet" . txt $ "."
renderTile _ Blinky     = withAttr "blinky" . txt $ " "
renderTile _ Inky       = withAttr "inky"   . txt $ " "
renderTile _ Pinky      = withAttr "pinky"  . txt $ " "
renderTile _ Clyde      = withAttr "clyde"  . txt $ " "
renderTile _ (Warp _ _) = withAttr "maze"   . txt $ " "
renderTile g Player     = withAttr "player" . txt . playerGlyph $ dir
    where dir = g ^. T.pacman . T.pdir

playerGlyph :: Direction -> Txt.Text
playerGlyph North = "∨"
playerGlyph South = "∧"
playerGlyph West  = ">"
playerGlyph East  = "<"

---------------------------------------------------------------------
-- Attributes

attributes :: AttrMap
attributes = attrMap V.defAttr
    [ ( "player", on V.black V.brightYellow )
    , ( "maze",   on V.blue V.black         )
    , ( "pellet", on V.white V.black        )
    , ( "score",  on V.white V.black        )
    , ( "blinky", bg V.red                  )
    , ( "pinky",  bg V.brightMagenta        )
    , ( "inky",   bg V.brightCyan           )
    , ( "clyde",  bg V.yellow               )
    , ( "background", bg V.black            )
    , ( borderAttr, on V.blue V.black       )
    ]
