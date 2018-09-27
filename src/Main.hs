{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import qualified Types        as T
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
import Controller                       ( eventRouter               )
import Types                            ( Game (..)
                                        , GameSt (..)
                                        , Tile (..)
                                        , Ghost (..)
                                        , Maze (..)
                                        , Direction (..)
                                        , Status (..)
                                        , TimeEvent (..)            )
import View                             ( drawUI
                                        , attributes                )
import Maze                             ( initGame
                                        , isWall                    )
import Brick.Types                      ( BrickEvent (..)
                                        , EventM
                                        , Next                      )
import Brick.Main                       ( App (..)
                                        , neverShowCursor
                                        , customMain                )

app :: App GameSt TimeEvent ()
app = App { appDraw         = drawUI
          , appHandleEvent  = eventRouter
          , appAttrMap      = const attributes
          , appStartEvent   = return
          , appChooseCursor = neverShowCursor }

main :: IO ()
main = do
    putEnv "TERM=xterm-256color"
    gen  <- getStdGen
    args <- getArgs
    etGame <- case args of
                   []  -> initGame gen <$> readFile "data/classicMaze1.txt"
                   x:_ -> initGame gen <$> readFile x
    case etGame of
         Left err -> putStrLn err
         Right g  -> runGame etGame

runGame :: GameSt -> IO ()
runGame etG = do
    chan <- newBChan 10 :: IO ( BChan TimeEvent )
    defaultConfig <- V.standardIOConfig
    forkIO . forever $ writeBChan chan Tick >> threadDelay 250000
    etG' <- customMain (V.mkVty defaultConfig) (Just chan) app etG
    case etG' of
         Left err -> putStrLn err
         Right g  -> case g ^. T.status of
                          GameOver  -> putStrLn "Game Over"
                          LevelOver -> putStrLn "Level Finished!"
                          otherwise -> putStrLn "Looks like you gave up..."
