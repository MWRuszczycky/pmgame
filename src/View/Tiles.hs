{-# LANGUAGE OverloadedStrings #-}

module View.Tiles
    ( renderPlayer
    , renderPwrPellet
    , renderTile
    , tileEdibleGhost
    , tileGhost
    ) where

import qualified Model.Types as T
import qualified Data.Text   as Txt
import Lens.Micro                   ( (^.)              )
import Brick.Types                  ( Widget (..)       )
import Brick.Widgets.Core           ( txt, withAttr     )
import Resources                    ( mazeNumber        )
import Model.Utilities              ( isFlashing
                                    , powerTimeLeft     )
import Model.Types                  ( Direction  (..)
                                    , FruitName  (..)
                                    , Game       (..)
                                    , Ghost      (..)
                                    , GhostName  (..)
                                    , GhostState (..)
                                    , Name       (..)
                                    , Tile       (..)   )

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Utilities for rendering and working with tiles

-- =============================================================== --
-- Tile generation utilities

tileGhost :: Game -> Ghost -> Tile
tileGhost gm g = case g ^. T.gstate of
                      Edible    -> tileEdibleGhost gm
                      EyesOnly  -> GhostEyes
                      otherwise -> NormalGhost $ g ^. T.gname

tileEdibleGhost :: Game -> Tile
tileEdibleGhost gm
    | trem >= half = BlueGhost
    | isWhite      = WhiteGhost
    | otherwise    = BlueGhost
    where trem    = powerTimeLeft gm
          half    = quot ( gm ^. T.pwrtime ) 2
          isWhite = isFlashing gm

-- =============================================================== --
-- Tile rendering

renderTile :: Game -> Tile -> Widget Name
renderTile gm (Wall s)              = renderWall (mazeNumber $ gm ^. T.level) s
renderTile _ (OneWay North)         = withAttr "oneway"     . txt $ "-"
renderTile _ (OneWay South)         = withAttr "oneway"     . txt $ "-"
renderTile _ (OneWay West )         = withAttr "oneway"     . txt $ "|"
renderTile _ (OneWay East )         = withAttr "oneway"     . txt $ "|"
renderTile _  Pellet                = withAttr "pellet"     . txt $ "."
renderTile gm PwrPellet             = renderPwrPellet gm
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
renderTile _ (FruitTile Galaxian  ) = withAttr "galaxian"   . txt $ "g"
renderTile _ (FruitTile Bell      ) = withAttr "bell"       . txt $ "b"
renderTile _ (FruitTile Key       ) = withAttr "key"        . txt $ "k"
renderTile gm Player                = renderPlayer gm
renderTile _  _                     = withAttr "maze1"      . txt $ " "

renderWall :: Int -> Txt.Text -> Widget Name
renderWall 1 = withAttr "maze1" . txt
renderWall 2 = withAttr "maze2" . txt
renderWall 3 = withAttr "maze3" . txt
renderWall 4 = withAttr "maze4" . txt
renderWall 5 = withAttr "maze5" . txt
renderWall _ = withAttr "maze1" . txt

renderPlayer :: Game -> Widget Name
renderPlayer g = withAttr "player" . txt . glyph $ g ^. T.pacman . T.pdir
    where glyph North = "∨"
          glyph South = "∧"
          glyph West  = ">"
          glyph East  = "<"

renderPwrPellet :: Game -> Widget Name
renderPwrPellet gm
    | isFlashing gm = withAttr "flashPwrPellet" . txt $ "●"
    | otherwise     = withAttr "pwrPellet"      . txt $ "●"
