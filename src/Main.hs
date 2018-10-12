{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import qualified Model.Types  as T
import Lens.Micro                       ( (^.)                      )
import System.Random                    ( getStdGen                 )
import System.Environment               ( getArgs                   )
import System.Posix.Env                 ( putEnv                    )
import Control.Monad                    ( void, forever             )
import Control.Concurrent               ( threadDelay
                                        , forkIO                    )
import Brick.BChan                      ( BChan
                                        , writeBChan
                                        , newBChan                  )
import Brick.Main                       ( App (..)
                                        , neverShowCursor
                                        , customMain                )
import Controller                       ( routeEvent                )
import Model.Types                      ( GameSt    (..)
                                        , Status    (..)
                                        , Time      (..)
                                        , TimeEvent (..)            )
import View                             ( drawUI
                                        , attributes                )
import Loading                          ( initGame                  )
import Model.Utilities                  ( tickPeriod                )

app :: App GameSt TimeEvent ()
app = App { appDraw         = drawUI
          , appHandleEvent  = routeEvent
          , appAttrMap      = const attributes
          , appStartEvent   = return
          , appChooseCursor = neverShowCursor }

main :: IO ()
main = do
    let dt    = tickPeriod
        mfile = "levels/classicMaze1.txt"
    putEnv "TERM=xterm-256color"
    gen  <- getStdGen
    args <- getArgs
    etGame <- case args of
                   []  -> initGame gen dt <$> readFile mfile
                   x:_ -> initGame gen dt <$> readFile x
    case etGame of
         Left err -> putStrLn err
         Right g  -> runGame dt etGame

runTimer :: BChan TimeEvent -> Time -> Time -> IO ()
runTimer chan t dt = do
    writeBChan chan (Tick t)
    threadDelay dt
    runTimer chan (t+dt) dt

runGame :: Time -> GameSt -> IO ()
runGame dt etG = do
    chan <- newBChan 10 :: IO ( BChan TimeEvent )
    defaultConfig <- V.standardIOConfig
    forkIO $ runTimer chan 0 dt
    etG' <- customMain (V.mkVty defaultConfig) (Just chan) app etG
    case etG' of
         Left msg -> putStrLn msg
         Right g  -> case g ^. T.status of
                          GameOver  -> putStrLn "Game Over"
                          LevelOver -> putStrLn "Level Finished!"
                          otherwise -> putStrLn "Looks like you gave up..."
