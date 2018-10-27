{-# LANGUAGE OverloadedStrings #-}

module View.View
    ( drawUI
    ) where

import qualified Model.Types as T
import Lens.Micro                   ( (^.)                  )
import Brick.Types                  ( Widget (..)           )
import Brick.Widgets.Core           ( str )
import View.MazeUIs                 ( drawDeathUI
                                    , drawPausedUI
                                    , drawRunningUI         )
import View.DialogUIs               ( drawStartScreenUI
                                    , drawGameOverUI
                                    , drawLevelOverUI
                                    , drawNewHighScoreUI
                                    , drawReplayUI          )
import Model.Types                  ( Game   (..)
                                    , GameSt (..)
                                    , Mode   (..)
                                    , Name   (..)
                                    , Time   (..)           )

type Renderer = Game -> [ Widget Name ]

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Interface to the View modules
-- Defines UI renderig for the game state

drawUI :: GameSt -> [ Widget Name ]
-- ^Entry point for rendering the game state.
drawUI (Left msg) = [ str msg ]
drawUI (Right gm) =
    case gm ^. T.mode of
         StartScreen    -> drawStartScreenUI gm
         GameOver t     -> routeTrans t drawDeathUI drawGameOverUI gm
         LevelOver      -> drawLevelOverUI gm
         NewHighScore t -> routeTrans t drawDeathUI drawNewHighScoreUI gm
         ReplayLvl t    -> routeTrans t drawDeathUI drawReplayUI gm
         Paused _       -> drawPausedUI gm
         otherwise      -> drawRunningUI gm

routeTrans :: Time -> Renderer -> Renderer -> Game -> [ Widget Name ]
routeTrans t inTrans postTrans gm
    | t > 0     = inTrans gm
    | otherwise = postTrans gm
