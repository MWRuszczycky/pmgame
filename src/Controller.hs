module Controller
    ( routeEvent
    ) where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import qualified Model.Types  as T
import Lens.Micro                   ( (&), (^.), (.~), over )
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
                                    , Mode       (..)
                                    , Name       (..)
                                    , TimeEvent  (..)       )
import Loading                      ( advanceLevel
                                    , levels
                                    , startNewGame          )
import Model.Model                  ( moveGhosts
                                    , movePlayer
                                    , restartLevel
                                    , updateGame
                                    , updateTime
                                    , updateTimePaused      )

type EventHandler = BrickEvent Name TimeEvent
                    -> EventM Name ( Next GameSt )
---------------------------------------------------------------------
-- Event routers

routeEvent :: GameSt -> EventHandler
routeEvent (Left err) _ = halt (Left err)
routeEvent (Right gm) e = case gm ^. T.mode of
                               GameOver     -> routeGameOver gm e
                               NewHighScore -> routeNewHighScore gm e
                               LevelOver    -> routeLevelOver gm e
                               ReplayLvl    -> routeReplay gm e
                               Paused m     -> routePaused gm m e
                               otherwise    -> routeRunning gm e

routeRunning :: Game -> EventHandler
routeRunning gm (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right $ gm
routeRunning gm (VtyEvent (V.EvKey (V.KChar ' ') [])) =
    continue . Right . pauseGame $ gm
routeRunning gm (VtyEvent (V.EvKey k ms)) =
    continue . Right . keyEvent k ms $ gm
routeRunning gm (AppEvent (Tick t)) =
    continue . Right . tickEvent t $ gm
routeRunning gm _ =
    continue . Right $ gm

routeReplay :: Game -> EventHandler
routeReplay gm (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right $ gm
routeReplay gm (VtyEvent (V.EvKey V.KEnter [])) =
    continue . Right . restartLevel $ gm
routeReplay gm _ =
    continue . Right $ gm

routeLevelOver :: Game -> EventHandler
routeLevelOver gm (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right $ gm
routeLevelOver gm (VtyEvent (V.EvKey V.KEnter [])) =
    suspendAndResume . startNextLevel gm
    $ lookup (succ $ gm ^. T.level) levels
routeLevelOver gm _ =
    continue . Right $ gm

routeNewHighScore :: Game -> EventHandler
routeNewHighScore gm (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right $ gm
routeNewHighScore gm (VtyEvent (V.EvKey V.KEnter [])) =
    suspendAndResume ( restartGame gm )
routeNewHighScore gm (VtyEvent vtyEv) = do
    newHsEdit <- handleEditorEvent vtyEv ( gm ^. T.hsedit )
    if (>26) . length . unlines . getEditContents $ newHsEdit
       then continue . Right $ gm
       else continue . Right $ gm & T.hsedit .~ newHsEdit
routeNewHighScore gm _ =
    continue . Right $ gm

routeGameOver :: Game -> EventHandler
routeGameOver gm (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right $ gm
routeGameOver gm (VtyEvent (V.EvKey V.KEnter [])) =
    suspendAndResume ( restartGame gm )
routeGameOver gm _ =
    continue . Right $ gm

routePaused :: Game -> Mode -> EventHandler
routePaused gm m (VtyEvent (V.EvKey V.KEsc [])) =
    halt . Right $ gm & T.mode .~ m
routePaused gm m (VtyEvent (V.EvKey (V.KChar ' ') [])) =
    continue . Right $ unpauseGame m gm
routePaused gm m (AppEvent (Tick t)) =
    continue . Right . pausedTickEvent t $ gm
routePaused gm _ _ =
    continue . Right $ gm

---------------------------------------------------------------------
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
-- Event handlers for paused game

pausedTickEvent :: Int -> Game -> Game
pausedTickEvent t gm = updateTimePaused t gm

unpauseGame :: Mode -> Game -> Game
unpauseGame m gm = gm & T.mode .~ m

---------------------------------------------------------------------
-- Event handlers for restarts

restartGame :: Game -> IO GameSt
restartGame g = do
    let gen = g ^. T.rgen
    case lookup 1 levels of
         Just fn -> startNewGame gen 1 <$> readFile fn
         Nothing -> return . Left $ "Cannot find first level"

---------------------------------------------------------------------
-- Level transitioning

startNextLevel :: Game -> Maybe FilePath -> IO GameSt
startNextLevel gm Nothing   = startNextLevel gm . lookup 1 $ levels
startNextLevel gm (Just fn) = advanceLevel gm <$> readFile fn
