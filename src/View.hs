{-# LANGUAGE OverloadedStrings #-}
module View
    ( drawUI
    , attributes
    ) where

import qualified Graphics.Vty as V
import qualified Data.Text    as T
import qualified Data.Matrix  as M
import Brick.Types                      ( Widget (..) )
import Brick.Widgets.Core               ( txt, withAttr, vBox, hBox )
import Brick.AttrMap                    ( attrMap, AttrMap )
import Brick.Util                       ( on, bg, fg )
import Brick.Widgets.Center             ( center )
import Types

drawUI :: Game -> [ Widget () ]
drawUI s = [ center . vBox $ [ hdr, m, scr ] ]
    where m   = vBox . map ( hBox . map (renderTile s) ) . M.toLists . maze $ s
          scr = withAttr "score" . txt . T.pack . show . score $ s
          hdr = withAttr "score" . txt $ "PacMan!"

renderTile :: Game -> Tile -> Widget ()
renderTile _ Empty  = withAttr "maze"   . txt $ " "
renderTile _ Pellet = withAttr "pellet" . txt $ "."
renderTile _ Blinky = withAttr "blinky" . txt $ " "
renderTile _ Inky   = withAttr "inky"   . txt $ " "
renderTile _ Pinky  = withAttr "pinky"  . txt $ " "
renderTile _ Clyde  = withAttr "clyde"  . txt $ " "
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
renderTile s Player = withAttr "player" . txt . playerGlyph $ s

playerGlyph :: Game -> T.Text
playerGlyph s = case direction s of
                     North -> "∨"
                     South -> "∧"
                     West  -> ">"
                     East  -> "<"

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
