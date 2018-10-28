{-# LANGUAGE OverloadedStrings #-}

module View.DialogUIs
    ( drawStartScreenUI
    , drawLevelOverUI
    , drawReplayUI
    , drawNewHighScoreUI
    , drawGameOverUI
    ) where

import qualified Data.Text    as Txt
import qualified Model.Types  as T
import Lens.Micro                       ( (.~), (^.), (&)           )
import Brick.Types                      ( Widget (..)               )
import Brick.Widgets.Core               ( (<+>), fill, hBox, hLimit
                                        , str, txt, vBox , vLimit
                                        , withAttr                  )
import Brick.Widgets.Edit               ( renderEditor              )
import Brick.Widgets.Center             ( hCenter                   )
import View.Tiles                       ( renderPwrPellet
                                        , renderTile
                                        , tileEdibleGhost           )
import View.Core                        ( putInDialogBox
                                        , renderVerticalSpace       )
import Model.Utilities                  ( addHighScore
                                        , highScore
                                        , maxDialogWidth
                                        , playerScore
                                        , scoreFruit                )
import Model.Types                      ( FruitName (..)
                                        , Game      (..)
                                        , HighScore (..)
                                        , Items     (..)
                                        , Mode      (..)
                                        , Name      (..)
                                        , Score     (..)            )

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Rendering dialog box-based UIs
-- These UIs are displayed when there is no gameplay, for example,
-- when the player has just started the game or completed a level.

-- =============================================================== --
-- Rendering the actual UIs
-- These functions are all exported.

drawStartScreenUI :: Game -> [ Widget Name ]
-- ^Player has not begun play and is at the start screen.
drawStartScreenUI gm =
    let parts = [
                  renderHighScoreDisplay gm
                , renderVerticalSpace 2
                , withAttr "focusControls" . txt $ " Enter to start a new game"
                , withAttr "focusControls" . txt $ " Esc to quit"
                , renderVerticalSpace 1
                , withAttr "controls" . txt $ " Arrow keys change direction"
                , withAttr "controls" . txt $ " Space to pause"
                , withAttr "controls" . txt $ " Esc to quit"
                ]
    in  putInDialogBox "PAC-MAN!" . vBox $ parts

drawLevelOverUI :: Game -> [ Widget Name ]
-- ^Player has completed a level.
drawLevelOverUI gm =
    let title = Txt.pack $ "LEVEL " ++ show (gm ^. T.level) ++ " COMPLETED!"
        parts = [
                  hCenter . renderLabeledScore $ gm
                , renderVerticalSpace 2
                , withAttr "focusControls" . txt $ " Enter to continue"
                , withAttr "focusControls" . txt $ " Esc to quit"
                ]
    in  putInDialogBox title . vBox $ parts

drawReplayUI :: Game -> [ Widget Name ]
-- ^Player has lost a life but still has remaining lives.
drawReplayUI gm =
    let parts = [
                  hCenter . renderLabeledScore $ gm
                , renderVerticalSpace 2
                , withAttr "focusControls" . txt $ " Enter to keep trying"
                , withAttr "focusControls" . txt $ " Esc to quit"
                ]
    in  putInDialogBox "YOU GOT CAPTURED!" . vBox $ parts

drawNewHighScoreUI :: Game -> [ Widget Name ]
-- ^Player has lost all lives and gotten a new high score.
drawNewHighScoreUI gm =
    let parts = [
                  renderHighScoreDisplay gm
                , renderVerticalSpace 1
                , hCenter . withAttr "highScore" . txt $ "Enter your name"
                , hCenter . hLimit 26 . renderEditor (str . unlines) True
                      $ gm ^. T.hsedit
                , renderVerticalSpace 1
                , renderScoreDetails gm
                , renderVerticalSpace 2
                , withAttr "controls" . txt $ " Enter to play again"
                , withAttr "controls" . txt $ " Esc to quit"
                ]
    in  putInDialogBox "NEW HIGH SCORE!" . vBox $ parts

drawGameOverUI :: Game -> [ Widget Name ]
-- ^Player has lost all lives and has not gotten a new high score.
drawGameOverUI gm =
    let parts = [
                  hCenter . withAttr "focusControls" . txt
                      $ " Enter to play again"
                , hCenter . withAttr "focusControls" . txt
                      $ " Esc to quit"
                ]
    in  putInDialogBox "GAME OVER!" . vBox $ parts

-- =============================================================== --
-- Functions for rendering the content of dialog boxes.
-- These are not exported.

---------------------------------------------------------------------
-- Rendering basic score information

renderHighScoreDisplay :: Game -> Widget Name
-- ^Summary of all recorded high scores and players' names. This
-- function will include a space for the player's name if an new
-- high score was achieved. It makes a call to renderHighScores for
-- the actual rendering.
renderHighScoreDisplay gm =
    let scores = gm ^. T.highscores
        score  = ( "Your name", playerScore gm )
    in  case gm ^. T.mode of
             NewHighScore _ -> renderHighScores . addHighScore score $ scores
             otherwise      -> renderHighScores scores

renderHighScores :: [HighScore] -> Widget Name
-- ^Render a list of high scores to a widget with a header. Display
-- a message if there are no high scores yet.
renderHighScores xs
    | null xs   = vBox [ hdr, hCenter noScoresMsg ]
    | otherwise = vBox [ hdr, scores      ]
    where hdr         = hCenter . withAttr "highScore" . txt $ "High Scores"
          scores      = vBox . map highScoreToWidget $ xs
          noScoresMsg = withAttr "info" . txt $ "No high scores yet!"

highScoreToWidget :: HighScore -> Widget Name
-- ^Format a high score as a widget. If the score cannot fit in the
-- dialog box, then the player's name is truncated.
highScoreToWidget = hCenter . withAttr "info" . str . go
    where go (name, s) | length name > available = truncated
                       | otherwise               = untruncated
                       where score       = show s
                             available   = maxDialogWidth - length score - 1
                             untruncated = name ++ " " ++ score
                             truncated   = take ( available - 2 ) name
                                           ++ ".. " ++ score

renderLabeledScore :: Game -> Widget Name
-- ^Simple display of score information.
renderLabeledScore gm
    | ps > hs   = hBox [ hsLabel, score ]
    | otherwise = hBox [ label,   score ]
    where ps      = playerScore gm
          hs      = highScore gm
          hsLabel = withAttr "score" . txt $ "New High Score: "
          label   = withAttr "score" . txt $ "Score: "
          score   = withAttr "score" . str . show . playerScore $ gm

---------------------------------------------------------------------
-- Rendering more complex score information that appears after the
-- player completes a game with a new high score.

renderScoreDetails :: Game -> Widget Name
-- ^Display a breakdown of the player's score.
renderScoreDetails gm =
    vBox [
           hCenter . renderPelletScoreHeader $ gm
         , renderPelletScores $ gm ^. T.items
         , renderVerticalSpace 1
         , hCenter . renderGhostScoreHeader $ gm
         , renderGhostScores  $ gm ^. T.items . T.gstscores
         , renderVerticalSpace 1
         , hCenter renderFruitScoreHeader
         , renderFruitScores  $ gm ^. T.items . T.fruits
         ]

renderPelletScoreHeader :: Game -> Widget Name
-- ^Header for pellet score section when rendering score details.
-- See also renderScoreDetails.
renderPelletScoreHeader gm =
    hBox [
           renderPwrPellet gm
         , withAttr "pelletText" . txt $ " Pellets "
         , renderPwrPellet gm
         ]

renderPelletScores :: Items -> Widget Name
-- ^Summary of score coming from all pellets eaten during gameplay.
-- See also renderScoreDetails.
renderPelletScores x
    | np + npp == 0 = withAttr "info" . txt $ "You didn't get any pellets!"
    | otherwise     = vBox [ go "Regular" np 10, go "Power" npp 50 ]
    where np       = x ^. T.pellets
          npp      = x ^. T.ppellets
          go p n s = hCenter . withAttr "score" . str
                     $ show n ++ " x " ++ p ++ " = " ++ show (n*s)

renderGhostScoreHeader :: Game -> Widget Name
-- ^Header for ghost score section when rendering score details.
-- See also renderScoreDetails.
renderGhostScoreHeader gm =
    hBox [
           renderTile gm . tileEdibleGhost $ gm
         , withAttr "ghostText" . txt $ "  Ghosts  "
         , renderTile gm . tileEdibleGhost $ gm
         ]

renderGhostScores :: [(Score, Int)] -> Widget Name
-- ^Summary of score coming from all ghosts captured during gameplay.
renderGhostScores [] = hCenter . withAttr "info" . txt $ "You didn't catch any!"
renderGhostScores gs = vBox . map (hCenter . withAttr "score" . str . go) $ gs
    where go (x, n) = concat [ show n ++ " x "
                             , show x ++ " = "
                             , show (n * x) ]

renderFruitScoreHeader :: Widget Name
-- ^Header for fruit score section when rendering score details.
-- See also renderScoreDetails.
renderFruitScoreHeader = withAttr "melon" . txt $ "Fruits"

renderFruitScores :: [(FruitName, Int)] -> Widget Name
-- ^Summary of score coming from all fruits eaten during gameplay.
-- See also renderScoreDetails.
renderFruitScores [] = hCenter . withAttr "info" . txt $ "You didn't get any!"
renderFruitScores xs = vBox . map renderFruitScore $ xs
    where renderFruitScore (x, n) =
            let go Cherry    = withAttr "cherry"     . str $ "Cherry"
                go Strawberry= withAttr "strawberry" . str $ "Strawberry"
                go Orange    = withAttr "orange"     . str $ "Orange"
                go Apple     = withAttr "apple"      . str $ "Apple"
                go Melon     = withAttr "melon"      . str $ "Melon"
                go Galaxian  = withAttr "galaxian"   . str $ "Galaxian"
                go Bell      = withAttr "bell"       . str $ "Bell"
                go Key       = withAttr "key"        . str $ "Key"
            in  hCenter . hBox
                $ [
                    withAttr "info" . str $ show n ++ " x "
                  , go x
                  , withAttr "info" . str $ " = " ++ show (n * scoreFruit x)
                  ]
