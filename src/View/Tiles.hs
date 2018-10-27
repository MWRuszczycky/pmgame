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
import Brick.AttrMap                ( AttrName (..)     )
import Resources                    ( mazeNumber        )
import Model.Utilities              ( isFlashing
                                    , powerTimeLeft     )
import Model.Types                  ( Direction  (..)
                                    , FruitName  (..)
                                    , Game       (..)
                                    , Ghost      (..)
                                    , GhostName  (..)
                                    , GhostState (..)
                                    , Mode       (..)
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
renderTile gm (Wall w)              = renderWall gm w
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
renderTile _  _                     = withAttr "background" . txt $ " "

---------------------------------------------------------------------
-- Maze rendering

renderWall :: Game -> Txt.Text -> Widget Name
renderWall gm w = let n = mazeNumber $ gm ^. T.level
                  in  case gm ^. T.mode of
                      ReplayLvl _    -> withAttr ( colorMaze 0 ) . txt $ w
                      GameOver _     -> withAttr ( colorMaze 0 ) . txt $ w
                      NewHighScore _ -> withAttr ( colorMaze 0 ) . txt $ w
                      otherwise      -> withAttr ( colorMaze n ) . txt $ w

colorMaze :: Int -> AttrName
colorMaze 0 = "deathMaze"
colorMaze 1 = "blueMaze"
colorMaze 2 = "pinkMaze"
colorMaze 3 = "cyanMaze"
colorMaze 4 = "redMaze"
colorMaze 5 = "whiteMaze"
colorMaze _ = "blueMaze"

---------------------------------------------------------------------
-- Rendering the player

renderPlayer :: Game -> Widget Name
renderPlayer gm = case gm ^. T.mode of
                       ReplayLvl _    -> renderDeadPlayer gm
                       GameOver _     -> renderDeadPlayer gm
                       NewHighScore _ -> renderDeadPlayer gm
                       otherwise      -> renderLivePlayer gm

renderLivePlayer :: Game -> Widget Name
-- ^Normal player.
renderLivePlayer gm = withAttr "player" . txt $ glyph
    where glyph = playerGlyph $ gm ^. T.pacman . T.pdir

renderDeadPlayer :: Game -> Widget Name
-- ^Player has been captured.
renderDeadPlayer gm
    | isFlashing gm = withAttr "player"     . txt $ glyph
    | otherwise     = withAttr "background" . txt $ " "
    where glyph = playerGlyph $ gm ^. T.pacman . T.pdir

playerGlyph :: Direction -> Txt.Text
playerGlyph North = "∨"
playerGlyph South = "∧"
playerGlyph West  = ">"
playerGlyph East  = "<"

---------------------------------------------------------------------
-- Power pellet rendering

renderPwrPellet :: Game -> Widget Name
renderPwrPellet gm
    | isFlashing gm = withAttr "flashPwrPellet" . txt $ "●"
    | otherwise     = withAttr "pwrPellet"      . txt $ "●"
