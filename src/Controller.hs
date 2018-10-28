module Controller
    ( routeEvent
    ) where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import qualified Model.Types  as T
import Lens.Micro                   ( (&), (^.), (.~), (%~) )
import Brick.Types                  ( BrickEvent (..)
                                    , EventM
                                    , Next                  )
import Brick.Widgets.Edit           ( getEditContents
                                    , handleEditorEvent     )
import Brick.Main                   ( continue
                                    , halt                  )
import Model.Types                  ( Direction  (..)
                                    , Game       (..)
                                    , GameSt     (..)
                                    , AsciiMaze  (..)
                                    , Mode       (..)
                                    , Name       (..)
                                    , Time       (..)
                                    , TimeEvent  (..)       )
import Loading                      ( advanceLevel
                                    , restartGame
                                    , startNewGame          )
import Model.Utilities              ( addHighScore
                                    , maxNameLength
                                    , playerScore
                                    , updateHighScores      )
import Model.Model                  ( moveGhosts
                                    , movePlayer
                                    , restartLevel
                                    , updateGame
                                    , updateClock
                                    , updateTime
                                    , updateTimePaused
                                    , updateTimeTrans       )

type EventHandler = BrickEvent Name TimeEvent
                    -> EventM Name ( Next GameSt )

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Event handlers for controlling the game and interfacing with Brick

-- =============================================================== --
-- Event routers

routeEvent :: GameSt -> EventHandler
routeEvent (Left err) _ = halt (Left err)
routeEvent (Right gm) e = case gm ^. T.mode of
                               StartScreen    -> routeStartScreen gm e
                               GameOver _     -> routeGameOver gm e
                               NewHighScore t -> routeNewHighScore t gm e
                               LevelOver      -> routeLevelOver gm e
                               ReplayLvl _    -> routeReplay gm e
                               Paused m       -> routePaused gm m e
                               otherwise      -> routeRunning gm e

routeStartScreen :: Game -> EventHandler
-- ^Start screen before starting a new game.
routeStartScreen gm (AppEvent (Tick t)) =
    continue . Right . updateClock t $ gm
routeStartScreen gm (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right $ gm
routeStartScreen gm (VtyEvent (V.EvKey V.KEnter [])) =
    continue . Right $ gm & T.mode .~ Running
routeStartScreen gm _ =
    continue . Right $ gm

routeRunning :: Game -> EventHandler
-- ^Normal, unpaused gameplay.
routeRunning gm (AppEvent (Tick t)) =
    continue . Right . tickEvent t $ gm
routeRunning gm (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right $ gm
routeRunning gm (VtyEvent (V.EvKey (V.KChar ' ') [])) =
    continue . Right . pauseGame $ gm
routeRunning gm (VtyEvent (V.EvKey k ms)) =
    continue . Right . keyEvent k ms $ gm
routeRunning gm _ =
    continue . Right $ gm

routePaused :: Game -> Mode -> EventHandler
-- ^Normal, paused gameplay.
routePaused gm m (AppEvent (Tick t)) =
    continue . Right . updateTimePaused t $ gm
routePaused gm m (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right $ gm & T.mode .~ m
routePaused gm m (VtyEvent (V.EvKey (V.KChar ' ') [])) =
    continue . Right $ gm & T.mode .~ m
routePaused gm _ _ =
    continue . Right $ gm

routeReplay :: Game -> EventHandler
-- ^Player still has lives left, but was just captured and is waiting
-- to replay the level.
routeReplay gm (AppEvent (Tick t)) =
    continue . Right . updateTimeTrans t $ gm
routeReplay gm (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right $ gm
routeReplay gm (VtyEvent (V.EvKey V.KEnter [])) =
    continue . Right . restartLevel $ gm
routeReplay gm _ =
    continue . Right $ gm

routeLevelOver :: Game -> EventHandler
-- ^Player has just completed a level and is waiting to start the
-- next level.
routeLevelOver gm (AppEvent (Tick t)) =
    continue . Right . updateClock t $ gm
routeLevelOver gm (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right $ gm
routeLevelOver gm (VtyEvent (V.EvKey V.KEnter [])) =
    continue . advanceLevel $ gm
routeLevelOver gm _ =
    continue . Right $ gm

routeNewHighScore :: Time -> Game -> EventHandler
-- ^Player has lost all lives, but has achieved a new high score and
-- is in the process of entering their name to be saved. If the
-- player tries to exit during the transition before entering a new
-- score, revert to the dialog immediately.
routeNewHighScore _ gm (AppEvent (Tick t)) =
    continue . Right . updateTimeTrans t $ gm
routeNewHighScore t gm (VtyEvent (V.EvKey V.KEsc []))
    | t > 0     = continue . Right $ gm & T.mode .~ NewHighScore 0
    | otherwise = halt . Right . updateHighScores $ gm
routeNewHighScore t gm (VtyEvent (V.EvKey V.KEnter []))
    | t > 0     = continue . Right $ gm & T.mode .~ NewHighScore 0
    | otherwise = continue . restartGame . updateHighScores $ gm
routeNewHighScore t gm (VtyEvent vtyEv)
    | t > 0     = continue . Right $ gm & T.mode .~ NewHighScore 0
    | otherwise = do
        newHsEdit <- handleEditorEvent vtyEv ( gm ^. T.hsedit )
        if (> maxNameLength+1) . length . unlines . getEditContents $ newHsEdit
           then continue . Right $ gm
           else continue . Right $ gm & T.hsedit .~ newHsEdit
routeNewHighScore _ gm _ =
    continue . Right $ gm

routeGameOver :: Game -> EventHandler
-- ^Player has lost all lives and has not achieved a new high score.
-- Player is currently deciding whether to try again or not.
routeGameOver gm (AppEvent (Tick t)) =
    continue . Right . updateTimeTrans t $ gm
routeGameOver gm (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right $ gm
routeGameOver gm (VtyEvent (V.EvKey V.KEnter [])) =
    continue . restartGame $ gm
routeGameOver gm _ =
    continue . Right $ gm

-- =============================================================== --
-- Event handlers for running game

tickEvent :: Time -> Game -> Game
tickEvent t gm = updateGame gm
                 . updateTime t
                 . moveGhosts
                 . movePlayer $ gm

pauseGame :: Game -> Game
pauseGame gm = let pausedMode = Paused $ gm ^. T.mode
               in  gm & T.mode .~ pausedMode

keyEvent :: V.Key -> [V.Modifier] -> Game -> Game
keyEvent  V.KLeft      _ gm = gm & T.pacman . T.pdir .~ West
keyEvent (V.KChar 'a') _ gm = gm & T.pacman . T.pdir .~ West
keyEvent  V.KRight     _ gm = gm & T.pacman . T.pdir .~ East
keyEvent (V.KChar 'd') _ gm = gm & T.pacman . T.pdir .~ East
keyEvent (V.KChar 'e') _ gm = gm & T.pacman . T.pdir .~ East
keyEvent  V.KUp        _ gm = gm & T.pacman . T.pdir .~ North
keyEvent (V.KChar 'w') _ gm = gm & T.pacman . T.pdir .~ North
keyEvent (V.KChar ',') _ gm = gm & T.pacman . T.pdir .~ North
keyEvent  V.KDown      _ gm = gm & T.pacman . T.pdir .~ South
keyEvent (V.KChar 's') _ gm = gm & T.pacman . T.pdir .~ South
keyEvent (V.KChar 'o') _ gm = gm & T.pacman . T.pdir .~ South
keyEvent _             _ gm = gm
