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
import Brick.Types                      ( Widget (..)               )
import Brick.Widgets.Core               ( txt, withAttr, vBox, hBox )
import Brick.AttrMap                    ( attrMap, AttrMap          )
import Brick.Util                       ( on, bg, fg                )
import Brick.Widgets.Center             ( center                    )
import Types                            ( Game (..)
                                        , Tile (..)
                                        , Direction (..)
                                        , Maze (..)                 )

drawUI :: Game -> [ Widget () ]
drawUI g = [ center . vBox $ [ hdr, m, scr ] ]
    where m   = renderMaze g
          scr = withAttr "score" . txt . Txt.pack . show $ g ^. T.score
          hdr = withAttr "score" . txt $ "PacMan!"

renderMaze :: Game -> Widget ()
renderMaze g = vBox . map ( hBox . map (renderTile g) ) . M.toLists $ m2
    where gs = g ^. T.ghosts
          m0 = g ^. T.maze
          m1 = M.setElem Player ( g ^. T.pacman . T.ppos ) m0
          m2 = foldl' (\ m x -> M.setElem (x ^. T.gname) (x ^. T.gpos) m) m1 gs

renderTile :: Game -> Tile -> Widget ()
renderTile _ Empty  = withAttr "maze"   . txt $ " "
renderTile _ HBar   = withAttr "maze"   . txt $ "═"
renderTile _ VBar   = withAttr "maze"   . txt $ "║"
renderTile _ Cros   = withAttr "maze"   . txt $ "╬"
renderTile _ LTee   = withAttr "maze"   . txt $ "╣"
renderTile _ RTee   = withAttr "maze"   . txt $ "╠"
renderTile _ DTee   = withAttr "maze"   . txt $ "╦"
renderTile _ UTee   = withAttr "maze"   . txt $ "╩"
renderTile _ LUCr   = withAttr "maze"   . txt $ "╔"
renderTile _ RUCr   = withAttr "maze"   . txt $ "╗"
renderTile _ LDCr   = withAttr "maze"   . txt $ "╚"
renderTile _ RDCr   = withAttr "maze"   . txt $ "╝"
renderTile _ Pellet = withAttr "pellet" . txt $ "."
renderTile _ Blinky = withAttr "blinky" . txt $ " "
renderTile _ Inky   = withAttr "inky"   . txt $ " "
renderTile _ Pinky  = withAttr "pinky"  . txt $ " "
renderTile _ Clyde  = withAttr "clyde"  . txt $ " "
renderTile g Player = withAttr "player" . txt . playerGlyph $ dir
    where dir = g ^. T.pacman . T.pdir

playerGlyph :: Direction -> Txt.Text
playerGlyph North = "∨"
playerGlyph South = "∧"
playerGlyph West  = ">"
playerGlyph East  = "<"

attributes :: AttrMap
attributes = attrMap V.defAttr [ ( "player", on V.black V.brightYellow  )
                               , ( "maze",   on V.blue V.black          )
                               , ( "pellet", on V.white V.black         )
                               , ( "score",  on V.yellow V.black        )
                               , ( "blinky", bg V.red                   )
                               , ( "pinky",  bg V.brightMagenta         )
                               , ( "inky",   bg V.brightCyan            )
                               , ( "clyde",  bg V.yellow                )
                               ]
