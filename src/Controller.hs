module Controller
    ( routeEvent
    ) where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import qualified Model.Types  as T
import Lens.Micro                   ( (&), (^.), (.~), over )
import Brick.Types                  ( BrickEvent (..)
                                    , Next
                                    , EventM                )
import Brick.Main                   ( continue
                                    , suspendAndResume
                                    , halt                  )
import Model.Types                  ( Game       (..)
                                    , Mode       (..)
                                    , TimeEvent  (..)
                                    , Direction  (..)
                                    , GameSt     (..)       )
import Loading                      ( levels
                                    , startNewGame          )
import Model.Model                  ( movePlayer
                                    , moveGhosts
                                    , restartLevel
                                    , updateTime
                                    , updateTimePaused
                                    , updateGame            )

type EventHandler = BrickEvent () TimeEvent -> EventM () ( Next GameSt )
---------------------------------------------------------------------
-- Event routers

routeEvent :: GameSt -> EventHandler
routeEvent (Left err) _ = halt (Left err)
routeEvent (Right gm) e = case gm ^. T.mode of
                               GameOver  -> routeGameOver gm e
                               LevelOver -> routeLevelOver gm e
                               ReplayLvl -> routeReplay gm e
                               Paused m  -> routePaused gm m e
                               otherwise -> routeRunning gm e

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
         Just fn -> startNewGame gen <$> readFile fn
         Nothing -> return . Left $ "Cannot find first level"

---------------------------------------------------------------------
-- Level transitioning

startNextLevel :: Game -> Maybe FilePath -> IO GameSt
startNextLevel g Nothing   = startNextLevel g . lookup 1 $ levels
startNextLevel g (Just fn) = do
    etG <- startNewGame ( g ^. T.rgen ) <$> readFile fn
    case etG of
         Left _   -> return etG
         Right g' -> return . Right $ g' & T.items  .~ ( g ^. T.items        )
                                         & T.level  .~ ( succ $ g ^. T.level )
                                         & T.oneups .~ ( g ^. T.oneups       )
                                         & T.time   .~ ( g ^. T.time         )
