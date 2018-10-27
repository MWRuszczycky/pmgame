{-# LANGUAGE OverloadedStrings #-}

module View.MazeUIs
    ( drawDeathUI
    , drawRunningUI
    , drawPausedUI
    ) where

import qualified Data.Matrix as M
import qualified Model.Types as T
import Lens.Micro                   ( (^.)                          )
import Data.List                    ( foldl'                        )
import Brick.Widgets.Core           ( (<+>), hBox, hLimit, padLeft
                                    , str, txt, vBox, vLimit
                                    , withAttr                      )
import Brick.Widgets.Center         ( center                        )
import Brick.Types                  ( Padding    (..)
                                    , Widget     (..)               )
import View.Core                    ( renderVerticalSpace           )
import View.Tiles                   ( renderTile, tileGhost         )
import Model.Utilities              ( highScore, playerScore        )
import Model.Types                  ( Direction  (..)
                                    , Fruit      (..)
                                    , FruitName  (..)
                                    , Game       (..)
                                    , GhostName  (..)
                                    , GhostState (..)
                                    , Maze       (..)
                                    , Message    (..)
                                    , Name       (..)
                                    , PacMan     (..)
                                    , Tile       (..)               )

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Gameplay UIs

-- =============================================================== --
-- Rendering the maze UIs for different modes

drawDeathUI :: Game -> [ Widget Name ]
-- ^Player has just been captured and this UI is letting the player
-- know it.
drawDeathUI gm =
    let width = M.ncols $ gm ^. T.maze
        parts = [
                  renderHeader gm
                , renderMaze tileDeathMaze gm
                , renderFooter gm
                , renderVerticalSpace 1
                , withAttr "controls" . txt $ " Space to pause"
                , withAttr "controls" . txt $ " Esc to quit"
                ]
    in  [ withAttr "background" . center . hLimit width . vBox $ parts ]

drawRunningUI :: Game -> [ Widget Name ]
-- ^Actual active gameplay UI.
drawRunningUI gm =
    let width = M.ncols $ gm ^. T.maze
        parts = [
                  renderHeader gm
                , renderMaze tileMaze gm
                , renderFooter gm
                , renderVerticalSpace 1
                , withAttr "controls" . txt $ " Space to pause"
                , withAttr "controls" . txt $ " Esc to quit"
                ]
    in  [ withAttr "background" . center . hLimit width . vBox $ parts ]

drawPausedUI :: Game -> [ Widget Name ]
-- ^Paused gameplay UI.
drawPausedUI gm =
    let width = M.ncols $ gm ^. T.maze
        parts = [
                  renderPausedHeader gm
                , renderMaze tileMaze gm
                , renderFooter gm
                , renderVerticalSpace 1
                , withAttr "controls" . txt $ " Space to unpause"
                , withAttr "controls" . txt $ " Esc to quit"
                ]
    in  [ withAttr "background" . center . hLimit width . vBox $ parts ]

-- =============================================================== --
-- Helper functions for construction of the gameplay UIs

renderMaze :: ( Game -> Maze ) -> Game -> Widget Name
-- ^Build and render the maze with fixed and changing tiles in place.
renderMaze tiler gm = vBox . renderTiles . M.toLists . tiler $ gm
    where renderTiles = map ( hBox . map (renderTile gm) )

---------------------------------------------------------------------
-- Mazes

tileMaze :: Game -> Maze
-- ^Standard maze tiling for normal gameplay.
tileMaze gm = tilePlayer   ( gm ^. T.pacman )
              . tileGhosts   gm
              . tileFruit  ( gm ^. T.fruit  )
              $ gm ^. T.maze

tileDeathMaze :: Game -> Maze
-- ^Maze tiling after player has been captured.
tileDeathMaze gm = tilePlayer ( gm ^. T.pacman )
                   . untilePellets
                   $ gm ^. T.maze

---------------------------------------------------------------------
-- Adding and removing tiles to and from mazes

tileFruit :: Maybe Fruit -> Maze -> Maze
tileFruit Nothing m = m
tileFruit (Just frt) m
    | isWaiting = m
    | otherwise = M.setElem frtTile (frt ^. T.fpos) m
    where isWaiting = frt ^. T.fdelay > 0
          frtTile   = FruitTile $ frt ^. T.fname

tileGhosts :: Game -> Maze -> Maze
tileGhosts gm m0 = foldl' ( \ m (p,t) -> M.setElem t p m) m0 gs
    where gs = [ (g ^. T.gpos, tileGhost gm g) | g <- gm ^. T.ghosts ]

tilePlayer :: PacMan -> Maze -> Maze
tilePlayer pm = M.setElem Player (pm ^. T.ppos)

untilePellets :: Maze -> Maze
-- ^Remove all pellets and power pellets from the maze.
untilePellets = M.mapPos go
    where go _ Pellet    = Empty
          go _ PwrPellet = Empty
          go _ x         = x

---------------------------------------------------------------------
-- Rendering scores and messages during regular gameplay
-- These are displayed in a header above the maze during gameplay.

renderScore :: Game -> Widget Name
-- ^Render the player's current score.
renderScore = withAttr "score" . str . show . playerScore

renderHighestScore :: Game -> Widget Name
-- ^Render the highest score recorded, which may be the player's
-- current score.
renderHighestScore gm
    | ps > hs   = withAttr "score" . str . show $ ps
    | otherwise = withAttr "score" . str . show $ hs
    where ps = playerScore gm
          hs = highScore gm

renderMessage :: Game -> Widget Name
-- ^Render messages during regular, unpaused gameplay, such as
-- whether a ghost was captured or a fruit was eaten. If there is
-- no message, then just display the current level.
renderMessage gm =
    let levelMsg = "Level " ++ show ( gm ^. T.level )
     in  case gm ^. T.msg of
              Message s _ -> withAttr "info" . str $ s
              otherwise   -> withAttr "info" . str $ levelMsg

renderHeader :: Game -> Widget Name
-- ^Collect together all the score and message information and
-- display above the maze during regular gameplay.
renderHeader gm = vLimit 3 . vBox $ [ row1, row2, row3 ]
    where row1 = padLeft Max . withAttr "score" . txt $ "High"
          row2 = hBox [ renderMessage gm, hsLabel ]
          row3 = hBox [ renderScore gm, padLeft Max . renderHighestScore $ gm ]
          hsLabel = padLeft Max . withAttr "score" . txt $ "Score"

renderPausedHeader :: Game -> Widget Name
-- ^Same as renderHeader but for paused gameplay.
renderPausedHeader gm = vLimit 3 . vBox $ [ row1, row2, row3 ]
    where row1 = padLeft Max . withAttr "score" . txt $ "High"
          row2 = hBox [ withAttr "info" . txt $ "PAUSED", hsLabel ]
          row3 = hBox [ renderScore gm, padLeft Max . renderHighestScore $ gm ]
          hsLabel = padLeft Max . withAttr "score" . txt $ "Score"

---------------------------------------------------------------------
-- Rendering fruit and oneups
-- These are displayed in a footer during gameplay.

renderFooter :: Game -> Widget Name
-- ^Graphical summary of remaining lives (oneups) and fruit eaten.
renderFooter gm = renderOneups gm <+> ( padLeft Max . renderFruitItems ) gm

renderOneups :: Game -> Widget Name
-- ^Graphical summary of remaining lives.
renderOneups gm
    | n > 0     = hBox . take n . cycle $ [ oneup, blank ]
    | otherwise = blank
    where n     = 2 * gm ^. T.oneups
          oneup = withAttr "player"     . txt $ ">"
          blank = withAttr "background" . txt $ " "

renderFruitItems :: Game -> Widget Name
-- ^Graphical summary of the different types of fruit eaten so far.
renderFruitItems gm = hBox . map ( renderTile gm . FruitTile ) $ names
    where names = fst . unzip $ gm ^. T.items . T.fruits
