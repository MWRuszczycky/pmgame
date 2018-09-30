module Controller
    ( eventRouter
    ) where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import qualified Types        as T
import Lens.Micro                   ( (&), (^.), (.~), set  )
import Brick.Types                  ( BrickEvent (..)
                                    , Next
                                    , EventM                )
import Brick.Main                   ( continue
                                    , suspendAndResume
                                    , halt                  )
import Types                        ( Game       (..)
                                    , Status     (..)
                                    , TimeEvent  (..)
                                    , Direction  (..)
                                    , GameSt     (..)       )
import Loading                      ( levels
                                    , initGame              )
import Model                        ( movePlayer
                                    , moveGhosts
                                    , restartLevel
                                    , getNxtLevel
                                    , updateGame            )

type EventHandler = BrickEvent () TimeEvent -> EventM () ( Next GameSt )
---------------------------------------------------------------------
-- Event routers

eventRouter :: GameSt -> EventHandler
eventRouter (Left err) _ = halt (Left err)
eventRouter (Right g)  e = case g ^. T.status of
                                GameOver     -> routeGameOver g e
                                LevelOver    -> routeLevelOver g e
                                ReplayLvl    -> routeReplay g e
                                otherwise    -> routeRunning g e

routeRunning :: Game -> EventHandler
routeRunning g (VtyEvent (V.EvKey V.KEsc [] )) =
    halt . Right $ g
routeRunning g (VtyEvent (V.EvKey k ms ))      =
    continue . Right . keyEvent k ms $ g
routeRunning g (AppEvent (Tick t))             =
    continue . Right . tickEvent t $ g
routeRunning g _                               =
    continue . Right $ g

routeReplay :: Game -> EventHandler
routeReplay g (VtyEvent (V.EvKey V.KEsc [] ))   =
    halt . Right $ g
routeReplay g (VtyEvent (V.EvKey V.KEnter [] )) =
    continue . Right . restartLevel $ g
routeReplay g _                                 =
    continue . Right $ g

routeLevelOver :: Game -> EventHandler
routeLevelOver g (VtyEvent (V.EvKey V.KEsc [] ))   =
    halt . Right $ g
routeLevelOver g (VtyEvent (V.EvKey V.KEnter [] )) =
    suspendAndResume . startNextLevel g $ lookup ( succ $ g ^. T.level ) levels
routeLevelOver g _                                 =
    continue . Right $ g

routeGameOver :: Game -> EventHandler
routeGameOver g (VtyEvent (V.EvKey V.KEsc [] ))   =
    halt . Right $ g
routeGameOver g (VtyEvent (V.EvKey V.KEnter [] )) =
    suspendAndResume ( restartGame g )
routeGameOver g (VtyEvent (V.EvResize _ _ ))      =
    continue . Right $ g
routeGameOver g _                                 =
    continue . Right $ g

---------------------------------------------------------------------
-- Event handlers for running game

tickEvent :: Int -> Game -> Game
tickEvent t g = updateGame g
                . set T.time t
                . moveGhosts
                . movePlayer $ g

keyEvent :: V.Key -> [V.Modifier] -> Game -> Game
keyEvent V.KLeft  ms g = g & T.pacman . T.pdir .~ West
keyEvent V.KRight ms g = g & T.pacman . T.pdir .~ East
keyEvent V.KUp    ms g = g & T.pacman . T.pdir .~ North
keyEvent V.KDown  ms g = g & T.pacman . T.pdir .~ South
keyEvent _        _  g = g

---------------------------------------------------------------------
-- Event handlers for restarts

restartGame :: Game -> IO GameSt
restartGame g = do
    let gen = g ^. T.rgen
        dt  = g ^. T.dtime
    case lookup 1 levels of
         Just fn -> initGame gen dt <$> readFile fn
         Nothing -> return . Left $ "Cannot find first level"

---------------------------------------------------------------------
-- Level transitioning

startNextLevel :: Game -> Maybe FilePath -> IO GameSt
startNextLevel g Nothing   = startNextLevel g . lookup 1 $ levels
startNextLevel g (Just fn) = do
    etG <- initGame ( g ^. T.rgen ) ( g ^. T.dtime ) <$> readFile fn
    case etG of
         Left _   -> return etG
         Right g' -> return . Right $ getNxtLevel g g'
