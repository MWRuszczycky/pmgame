{-# LANGUAGE OverloadedStrings #-}
module View
    ( drawUI
    , attributes
    ) where

import qualified Graphics.Vty as V
import qualified Data.Text    as T
import qualified Data.Matrix  as M
import Brick.Types                      ( Widget (..)               )
import Brick.Widgets.Core               ( txt, withAttr, vBox, hBox )
import Brick.AttrMap                    ( attrMap, AttrMap          )
import Brick.Util                       ( on, bg, fg                )
import Brick.Widgets.Center             ( center                    )
import Types

drawUI :: Game -> [ Widget () ]
drawUI s = [ center . vBox $ [ hdr, m, scr ] ]
    where m   = vBox . map ( hBox . map (renderTile s) ) . M.toLists . maze $ s
          scr = withAttr "score" . txt . T.pack . show . score $ s
          hdr = withAttr "score" . txt $ "PacMan!"

renderTile :: Game -> [Tile] -> Widget ()
renderTile _ []         = withAttr "maze"   . txt $ " "
renderTile _ (HBar:_)   = withAttr "maze"   . txt $ "═"
renderTile _ (VBar:_)   = withAttr "maze"   . txt $ "║"
renderTile _ (Cros:_)   = withAttr "maze"   . txt $ "╬"
renderTile _ (LTee:_)   = withAttr "maze"   . txt $ "╣"
renderTile _ (RTee:_)   = withAttr "maze"   . txt $ "╠"
renderTile _ (DTee:_)   = withAttr "maze"   . txt $ "╦"
renderTile _ (UTee:_)   = withAttr "maze"   . txt $ "╩"
renderTile _ (LUCr:_)   = withAttr "maze"   . txt $ "╔"
renderTile _ (RUCr:_)   = withAttr "maze"   . txt $ "╗"
renderTile _ (LDCr:_)   = withAttr "maze"   . txt $ "╚"
renderTile _ (RDCr:_)   = withAttr "maze"   . txt $ "╝"
renderTile s (Player:_) = withAttr "player" . txt . playerGlyph . pdir $ s
renderTile _ (Pellet:_) = withAttr "pellet" . txt $ "."
renderTile _ (Blinky:_) = withAttr "blinky" . txt $ " "
renderTile _ (Inky:_)   = withAttr "inky"   . txt $ " "
renderTile _ (Pinky:_)  = withAttr "pinky"  . txt $ " "
renderTile _ (Clyde:_)  = withAttr "clyde"  . txt $ " "

playerGlyph :: Direction -> T.Text
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
