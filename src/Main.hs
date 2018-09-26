{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import qualified Types        as T
import Lens.Micro                       ( (^.)                      )
import System.Random                    ( getStdGen                 )
import System.Posix.Env                 ( putEnv                    )
import Control.Monad                    ( void, forever             )
import Control.Concurrent               ( threadDelay
                                        , forkIO                    )
import Brick.BChan                      ( BChan
                                        , writeBChan
                                        , newBChan                  )
import Controller                       ( eventRouter               )
import Types                            ( Game (..)
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

app :: App Game TimeEvent ()
app = App { appDraw         = drawUI
          , appHandleEvent  = eventRouter
          , appAttrMap      = const attributes
          , appStartEvent   = return
          , appChooseCursor = neverShowCursor }

main :: IO ()
main = do
    putEnv "TERM=xterm-256color"
    gen  <- getStdGen
    mbGame <- initGame gen <$> readFile "data/classicMaze1.txt"
    case mbGame of
         Nothing -> putStrLn "Maze cannot be loaded."
         Just g  -> runGame g

runGame :: Game -> IO ()
runGame g = do
    chan <- newBChan 10 :: IO ( BChan TimeEvent )
    defaultConfig <- V.standardIOConfig
    forkIO . forever $ writeBChan chan Tick >> threadDelay 250000
    g' <- customMain (V.mkVty defaultConfig) (Just chan) app g
    case g' ^. T.status of
         GameOver  -> putStrLn "Game Over"
         LevelOver -> putStrLn "Level Finished!"
         otherwise -> putStrLn "Looks like you gave up..."
