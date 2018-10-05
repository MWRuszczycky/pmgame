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
import Model                            ( isGhost ,isWall, isPlayer
                                        , isPellet, tileGhosts      )
import Types                            ( Game      (..)
                                        , GameSt    (..)
                                        , Tile      (..)
                                        , Direction (..)
                                        , Status    (..)
                                        , Maze      (..)            )

drawUI :: GameSt -> [ Widget () ]
drawUI (Left msg) = [ str msg ]
drawUI (Right g)  = case g ^. T.status of
                        GameOver     -> drawGameOverUI g
                        LevelOver    -> drawLevelOverUI g
                        ReplayLvl    -> drawReplayUI g
                        otherwise    -> drawRunningUI g

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
renderScore g = withAttr "score" . str . show $ pel + gst + ppel
    where pel  = 10 * g ^. T.items . T.pellets
          ppel = 50 * g ^. T.items . T.ppellets
          gst  = g ^. T.items . T.gstscore

renderOneups :: Game -> Widget ()
renderOneups g = hBox . take (2 * g ^. T.oneups) . cycle $ [ oneup, space ]
    where oneup = withAttr "player" . txt $ ">"
          space = withAttr "background" . txt $ " "

renderFruit :: Game -> Widget ()
renderFruit g = padLeft Max . withAttr "score" . txt $ "fruit!"

renderMaze :: Game -> Widget ()
renderMaze g = vBox . map ( hBox . map (renderTile g) ) . M.toLists $ m2
    where gs = tileGhosts g
          m0 = g ^. T.maze
          m1 = M.setElem Player ( g ^. T.pacman . T.ppos ) m0
          m2 = foldl' (\ m (p,t) -> M.setElem t p m) m1 gs

renderTile :: Game -> Tile -> Widget ()
renderTile g t
    | isWall t   = renderWall t
    | isPellet t = renderPellet t
    | isGhost t  = renderGhost t
    | isPlayer t = renderPlayer g
    | otherwise  = withAttr "maze" . txt $ " "

renderWall :: Tile -> Widget ()
renderWall HBar = withAttr "maze" . txt $ "═"
renderWall VBar = withAttr "maze" . txt $ "║"
renderWall Cros = withAttr "maze" . txt $ "╬"
renderWall LTee = withAttr "maze" . txt $ "╣"
renderWall RTee = withAttr "maze" . txt $ "╠"
renderWall DTee = withAttr "maze" . txt $ "╦"
renderWall UTee = withAttr "maze" . txt $ "╩"
renderWall LUCr = withAttr "maze" . txt $ "╔"
renderWall RUCr = withAttr "maze" . txt $ "╗"
renderWall LDCr = withAttr "maze" . txt $ "╚"
renderWall RDCr = withAttr "maze" . txt $ "╝"

renderPellet :: Tile -> Widget ()
renderPellet Pellet    = withAttr "pellet" . txt $ "."
renderPellet PwrPellet = withAttr "pellet" . txt $ "*"

renderPlayer :: Game -> Widget ()
renderPlayer g = withAttr "player" . txt . glyph $ g ^. T.pacman . T.pdir
    where glyph North = "∨"
          glyph South = "∧"
          glyph West  = ">"
          glyph East  = "<"

renderGhost :: Tile -> Widget ()
renderGhost BlueGhost  = withAttr "blueGhost"  . txt $ "\""
renderGhost WhiteGhost = withAttr "whiteGhost" . txt $ "\""
renderGhost Blinky     = withAttr "blinky"     . txt $ "\""
renderGhost Inky       = withAttr "inky"       . txt $ "\""
renderGhost Pinky      = withAttr "pinky"      . txt $ "\""
renderGhost Clyde      = withAttr "clyde"      . txt $ "\""

---------------------------------------------------------------------
-- Attributes

attributes :: AttrMap
attributes = attrMap V.defAttr
    [ ( "player", on V.black V.brightYellow )
    , ( "maze",   on V.blue V.black         )
    , ( "pellet", on V.white V.black        )
    , ( "score",  on V.white V.black        )
    , ( "blinky", on V.black V.red          )
    , ( "pinky", on V.black V.brightMagenta )
    , ( "inky",   on V.black V.brightCyan   )
    , ( "clyde",  on V.black V.yellow       )
    , ( "blueGhost",  on V.white V.blue     )
    , ( "whiteGhost", on V.black V.white    )
    , ( "background", bg V.black            )
    , ( borderAttr,   on V.blue V.black     )
    ]
