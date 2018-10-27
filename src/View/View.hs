{-# LANGUAGE OverloadedStrings #-}

module View.View
    ( drawUI
    ) where

import qualified Model.Types as T
import Lens.Micro                   ( (^.)                  )
import Brick.Types                  ( Widget (..)           )
import Brick.Widgets.Core           ( str )
import View.MazeUIs                 ( drawPausedUI
                                    , drawRunningUI         )
import View.DialogUIs               ( drawStartScreenUI
                                    , drawGameOverUI
                                    , drawLevelOverUI
                                    , drawNewHighScoreUI
                                    , drawReplayUI          )
import Model.Types                  ( GameSt (..)
                                    , Mode   (..)
                                    , Name   (..)           )

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Interface to the View modules
-- Defines UI renderig for the game state

drawUI :: GameSt -> [ Widget Name ]
-- ^Entry point for rendering the game state.
drawUI (Left msg) = [ str msg ]
drawUI (Right gm) = case gm ^. T.mode of
                         StartScreen  -> drawStartScreenUI gm
                         GameOver     -> drawGameOverUI gm
                         LevelOver    -> drawLevelOverUI gm
                         NewHighScore -> drawNewHighScoreUI gm
                         ReplayLvl    -> drawReplayUI gm
                         Paused _     -> drawPausedUI gm
                         otherwise    -> drawRunningUI gm
