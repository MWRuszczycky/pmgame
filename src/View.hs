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
import Model.Model                      ( tileGhosts                )
import Model.Utilities                  ( isGhost                   )
import Model.Types                      ( Game      (..)
                                        , GameSt    (..)
                                        , Tile      (..)
                                        , Fruit     (..)
                                        , Direction (..)
                                        , Status    (..)
                                        , Maze      (..)            )

drawUI :: GameSt -> [ Widget () ]
drawUI (Left msg) = [ str msg ]
drawUI (Right g)  = case g ^. T.status of
                         GameOver  -> drawGameOverUI g
                         LevelOver -> drawLevelOverUI g
                         ReplayLvl -> drawReplayUI g
                         otherwise -> drawRunningUI g

drawRunningUI :: Game -> [ Widget () ]
drawRunningUI g = [ withAttr "background" ui ]
    where ui = center . hLimit ( M.ncols $ g ^. T.maze ) . vBox $ ws
          ws = [ renderHeader g
               , renderMaze g
               , renderOneups g <+> renderFruit g
               ]

drawGameOverUI :: Game -> [ Widget () ]
drawGameOverUI g = [ withAttr "background" msg ]
    where hdr = withAttr "info" . txt $ "GAME OVER!"
          ent = withAttr "info" . txt $ "Enter to play again"
          esc = withAttr "info" . txt $ "Esc to quit"
          msg = center
                . withBorderStyle unicodeRounded
                . borderWithLabel hdr
                . hLimit 25
                . vLimit 3
                . center
                . vBox $ [ renderScore g, ent, esc ]

drawLevelOverUI :: Game -> [ Widget () ]
drawLevelOverUI g = [ withAttr "background" msg]
    where hdr = withAttr "info" . txt $ "LEVEL COMPLETED!"
          ent = withAttr "info" . txt $ "Enter to play next level"
          esc = withAttr "info" . txt $ "Esc to quit"
          msg = center
                . withBorderStyle unicodeRounded
                . borderWithLabel hdr
                . hLimit 25
                . vLimit 3
                . center
                . vBox $ [ renderScore g, ent, esc ]

drawReplayUI :: Game -> [ Widget () ]
drawReplayUI g = [ withAttr "background" msg]
    where hdr = withAttr "info" . txt $ "YOU GOT CAPTURED!"
          ent = withAttr "info" . txt $ "Enter to keep trying"
          esc = withAttr "info" . txt $ "Esc to quit"
          msg = center
                . withBorderStyle unicodeRounded
                . borderWithLabel hdr
                . hLimit 25
                . vLimit 3
                . center
                . vBox $ [ renderScore g, ent, esc ]

---------------------------------------------------------------------
-- Widget rendering

renderHeader :: Game -> Widget ()
renderHeader g = vLimit 3 . vBox $ [ fstRow, sndRow, thdRow ]
    where fstRow  = padLeft Max . withAttr "score" . txt $ "High"
          sndRow  = hBox [ renderMessage g, hsLabel ]
          thdRow  = hBox [ renderScore g, padLeft Max . renderHighScore $ g ]
          hsLabel = padLeft Max . withAttr "score" . txt $ "Score"

renderHighScore :: Game -> Widget ()
renderHighScore g = renderScore g

renderScore :: Game -> Widget ()
renderScore g = withAttr "score" . str . show $ pel + gst + ppel + frt
    where pel  = 10 * g ^. T.items . T.pellets
          ppel = 50 * g ^. T.items . T.ppellets
          gst  = g ^. T.items . T.gstscore
          frt  = foldl' ( \ s (_,fs) -> s + fs ) 0 $ g ^. T.items . T.fruits

renderOneups :: Game -> Widget ()
renderOneups g = hBox . take (2 * g ^. T.oneups) . cycle $ [ oneup, space ]
    where oneup = withAttr "player" . txt $ ">"
          space = withAttr "background" . txt $ " "

renderFruit :: Game -> Widget ()
renderFruit g = padLeft Max . withAttr "score" . txt $ "fruit!"

renderMessage :: Game -> Widget ()
renderMessage g = go $ g ^. T.msg
    where time            = quot (g ^. T.time) 1000000
          go Nothing      = withAttr "info" . str . show $ time
          go (Just (s,_)) = withAttr "info" . str $ s

renderMaze :: Game -> Widget ()
renderMaze g = vBox . map ( hBox . map (renderTile g) ) . M.toLists $ m3
    where gs = tileGhosts g
          m0 = g ^. T.maze
          m1 = addFruit (g ^. T.fruit) m0
          m2 = foldl' (\ m (p,t) -> M.setElem t p m) m1 gs
          m3 = M.setElem Player ( g ^. T.pacman . T.ppos ) m2

addFruit :: Maybe Fruit -> Maze -> Maze
addFruit Nothing m = m
addFruit (Just frt) m
    | isWaiting = m
    | otherwise = M.setElem (frt ^. T.fname) (frt ^. T.fpos) m
    where isWaiting = frt ^. T.fdelay > 0

renderTile :: Game -> Tile -> Widget ()
renderTile _ (Wall s)       = withAttr "maze"       . txt $ s
renderTile _ (OneWay North) = withAttr "oneway"     . txt $ "-"
renderTile _ (OneWay South) = withAttr "oneway"     . txt $ "-"
renderTile _ (OneWay West)  = withAttr "oneway"     . txt $ "|"
renderTile _ (OneWay East)  = withAttr "oneway"     . txt $ "|"
renderTile _  Pellet         = withAttr "pellet"     . txt $ "."
renderTile _  PwrPellet      = withAttr "pellet"     . txt $ "*"
renderTile _  BlueGhost      = withAttr "blueGhost"  . txt $ "\""
renderTile _  WhiteGhost     = withAttr "whiteGhost" . txt $ "\""
renderTile _  Blinky         = withAttr "blinky"     . txt $ "\""
renderTile _  Inky           = withAttr "inky"       . txt $ "\""
renderTile _  Pinky          = withAttr "pinky"      . txt $ "\""
renderTile _  Clyde          = withAttr "clyde"      . txt $ "\""
renderTile _  GhostEyes      = withAttr "ghostEyes"  . txt $ "\""
renderTile _  Cherry         = withAttr "cherry"     . txt $ "c"
renderTile _  Strawberry     = withAttr "strawberry" . txt $ "s"
renderTile _  Orange         = withAttr "orange"     . txt $ "o"
renderTile _  Apple          = withAttr "apple"      . txt $ "a"
renderTile _  Melon          = withAttr "melon"      . txt $ "m"
renderTile gm Player         = renderPlayer gm
renderTile _  _              = withAttr "maze"       . txt $ " "

renderPlayer :: Game -> Widget ()
renderPlayer g = withAttr "player" . txt . glyph $ g ^. T.pacman . T.pdir
    where glyph North = "∨"
          glyph South = "∧"
          glyph West  = ">"
          glyph East  = "<"

---------------------------------------------------------------------
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
