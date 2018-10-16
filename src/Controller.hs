module Controller
    ( readFileEither
    , routeEvent
    ) where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import qualified Model.Types  as T
import Control.Exception            ( IOException, catch  )
import Lens.Micro                   ( (&), (^.), (.~), (%~) )
import Brick.Types                  ( BrickEvent (..)
                                    , EventM
                                    , Next                  )
import Brick.Widgets.Edit           ( getEditContents
                                    , handleEditorEvent     )
import Brick.Main                   ( continue
                                    , halt
                                    , suspendAndResume      )
import Model.Types                  ( Direction  (..)
                                    , Game       (..)
                                    , GameSt     (..)
                                    , MazeString (..)
                                    , Mode       (..)
                                    , Name       (..)
                                    , TimeEvent  (..)       )
import Loading                      ( advanceLevel
                                    , getLevelFile
                                    , restartNewGame
                                    , startNewGame          )
import Model.Utilities              ( addHighScore
                                    , playerScore           )
import Model.Model                  ( moveGhosts
                                    , movePlayer
                                    , restartLevel
                                    , updateGame
                                    , updateHighScores
                                    , updateClock
                                    , updateTime
                                    , updateTimePaused      )

type EventHandler = BrickEvent Name TimeEvent
                    -> EventM Name ( Next GameSt )

---------------------------------------------------------------------
---------------------------------------------------------------------
-- IO actions and event handlers for controlling the game

-- =============================================================== --
-- IO helper functions

readFileEither :: FilePath-> IO (Either String String)
readFileEither fp = do
    catch ( Right <$> readFile fp ) ( hndlErr fp )
    where hndlErr :: FilePath -> IOException -> IO (Either String String)
          hndlErr x _ = return . Left $ "Error: cannot find file " ++ x

-- =============================================================== --
-- Event routers

routeEvent :: GameSt -> EventHandler
routeEvent (Left err) _ = halt (Left err)
routeEvent (Right gm) e = case gm ^. T.mode of
                               StartScreen  -> routeStartScreen gm e
                               GameOver     -> routeGameOver gm e
                               NewHighScore -> routeNewHighScore gm e
                               LevelOver    -> routeLevelOver gm e
                               ReplayLvl    -> routeReplay gm e
                               Paused m     -> routePaused gm m e
                               otherwise    -> routeRunning gm e

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
routeReploy gm (AppEvent (Tick t)) =
    continue . Right . updateClock t $ gm
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
    suspendAndResume . startNextLevel $ gm
routeLevelOver gm _ =
    continue . Right $ gm

routeNewHighScore :: Game -> EventHandler
-- ^Player has lost all lives, but has achieved a new high score and
-- is in the process of entering their name to be saved.
routeNewHighScore gm (AppEvent (Tick t)) =
    continue . Right . updateClock t $ gm
routeNewHighScore gm (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right . updateHighScores $ gm
routeNewHighScore gm (VtyEvent (V.EvKey V.KEnter [])) = do
    suspendAndResume . restartGame . updateHighScores $ gm
routeNewHighScore gm (VtyEvent vtyEv) = do
    newHsEdit <- handleEditorEvent vtyEv ( gm ^. T.hsedit )
    if (>26) . length . unlines . getEditContents $ newHsEdit
       then continue . Right $ gm
       else continue . Right $ gm & T.hsedit .~ newHsEdit
routeNewHighScore gm _ =
    continue . Right $ gm

routeGameOver :: Game -> EventHandler
-- ^Player has lost all lives and has not achieved a new high score.
-- Player is currently deciding whether to try again or not.
routeGameOver gm (AppEvent (Tick t)) =
    continue . Right . updateClock t $ gm
routeGameOver gm (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right $ gm
routeGameOver gm (VtyEvent (V.EvKey V.KEnter [])) =
    suspendAndResume . restartGame $ gm
routeGameOver gm _ =
    continue . Right $ gm

-- =============================================================== --
-- Event handlers for running game

tickEvent :: Int -> Game -> Game
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

---------------------------------------------------------------------
-- Starting & restarting the game.

restartGame :: Game -> IO GameSt
restartGame gm = do
    ms <- readFileEither . getLevelFile $ 1
    return $ restartNewGame gm =<< ms

---------------------------------------------------------------------
-- Level transitioning

startNextLevel :: Game -> IO GameSt
startNextLevel gm = do
    ms <- readFileEither . getLevelFile . succ $ gm ^. T.level
    return $ advanceLevel gm =<< ms
