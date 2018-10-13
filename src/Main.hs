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
                                        , Mode      (..)
                                        , Time      (..)
                                        , TimeEvent (..)            )
import View                             ( drawUI
                                        , attributes                )
import Loading                          ( startNewGame              )
import Model.Utilities                  ( tickPeriod                )

app :: App GameSt TimeEvent ()
app = App { appDraw         = drawUI
          , appHandleEvent  = routeEvent
          , appAttrMap      = const attributes
          , appStartEvent   = return
          , appChooseCursor = neverShowCursor }

main :: IO ()
main = do
    let mfile = "levels/classicMaze1.txt"
    putEnv "TERM=xterm-256color"
    gen  <- getStdGen
    args <- getArgs
    etGame <- case args of
                   []  -> startNewGame gen <$> readFile mfile
                   x:_ -> startNewGame gen <$> readFile x
    case etGame of
         Left err -> putStrLn err
         Right g  -> runGame etGame

runTimer :: BChan TimeEvent -> Time -> IO ()
runTimer chan t = do
    writeBChan chan (Tick t)
    threadDelay tickPeriod
    runTimer chan (t + tickPeriod)

runGame :: GameSt -> IO ()
runGame etG = do
    chan <- newBChan 10 :: IO ( BChan TimeEvent )
    defaultConfig <- V.standardIOConfig
    forkIO $ runTimer chan 0
    etG' <- customMain (V.mkVty defaultConfig) (Just chan) app etG
    case etG' of
         Left msg -> putStrLn msg
         Right g  -> case g ^. T.mode of
                          GameOver  -> putStrLn "Game Over"
                          LevelOver -> putStrLn "Level Finished!"
                          otherwise -> putStrLn "Looks like you gave up..."
