{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import System.Posix.Env                 ( putEnv            )
import Control.Monad                    ( void, forever     )
import Control.Concurrent               ( threadDelay
                                        , forkIO            )
import Brick.BChan                      ( BChan
                                        , writeBChan
                                        , newBChan          )
import Controller                       ( eventRouter       )
import Types                            ( Game (..)
                                        , Tile (..)
                                        , Direction (..)
                                        , TimeEvent (..)    )
import View                             ( drawUI
                                        , attributes        )
import Maze                             ( initMaze
                                        , isWall            )
import Brick.Types                      ( BrickEvent (..)
                                        , EventM
                                        , Next              )
import Brick.Main                       ( App (..)
                                        , neverShowCursor
                                        , customMain        )

app :: App Game TimeEvent ()
app = App { appDraw         = drawUI
          , appHandleEvent  = eventRouter
          , appAttrMap      = const attributes
          , appStartEvent   = return
          , appChooseCursor = neverShowCursor }

start :: M.Matrix Tile -> Game
start m = Game { maze = m, score = 0, direction = North, ghosts = gs }
    where gs = [ ( Blinky, West ) ]
               -- , ( Inky, North  )
               -- , ( Pinky, East  )
               -- , ( Clyde, South ) ]

main :: IO ()
main = do
    putEnv "TERM=xterm-256color"
    m <- initMaze <$> readFile "data/classicMaze1.txt"
    chan <- newBChan 10 :: IO ( BChan TimeEvent )
    defaultConfig <- V.standardIOConfig
    forkIO . forever $ writeBChan chan Tick >> threadDelay 250000
    void $ customMain (V.mkVty defaultConfig) (Just chan) app (start m)
