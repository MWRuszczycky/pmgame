{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Graphics.Vty as V
import qualified Data.Matrix  as M
import qualified Model.Types  as T
import Lens.Micro                       ( (^.)                      )
import System.Directory                 ( doesFileExist             )
import System.Random                    ( getStdGen                 )
import System.Environment               ( getArgs                   )
import System.Posix.Env                 ( putEnv                    )
import Control.Monad                    ( void, forever             )
import Control.Concurrent               ( threadDelay
                                        , forkIO                    )
import Text.Read                        ( readMaybe                 )
import Brick.BChan                      ( BChan
                                        , newBChan
                                        , writeBChan                )
import Brick.Main                       ( App (..)
                                        , customMain
                                        , showCursorNamed           )
import Controller                       ( readFileEither
                                        , routeEvent                )
import Model.Types                      ( GameSt    (..)
                                        , HighScore (..)
                                        , Mode      (..)
                                        , Name      (..)
                                        , Time      (..)
                                        , TimeEvent (..)            )
import View                             ( attributes
                                        , drawUI                    )
import Loading                          ( getLevelFile
                                        , highScoresFile
                                        , readHighScores
                                        , readLevel
                                        , showHighScore
                                        , startNewGame              )
import Model.Utilities                  ( tickPeriod                )

app :: App GameSt TimeEvent Name
app = App { appDraw         = drawUI
          , appHandleEvent  = routeEvent
          , appAttrMap      = const attributes
          , appStartEvent   = return
          , appChooseCursor = const (showCursorNamed HighScoreEdit) }

runTimer :: BChan TimeEvent -> Time -> IO ()
runTimer chan t = do
    writeBChan chan (Tick t)
    threadDelay tickPeriod
    runTimer chan (t + tickPeriod)

main :: IO ()
main = do
    putEnv "TERM=xterm-256color"
    level   <- readLevel <$> getArgs
    gen     <- getStdGen
    mazeStr <- readFileEither . getLevelFile $ level
    scores  <- readHighScores <$> readFileEither highScoresFile
    runGame $ mazeStr >>= startNewGame gen scores level

runGame :: GameSt -> IO ()
runGame (Left err) = putStrLn err
runGame newGame = do
    chan <- newBChan 10 :: IO ( BChan TimeEvent )
    defaultConfig <- V.standardIOConfig
    forkIO $ runTimer chan 0
    finishedGame <- customMain (V.mkVty defaultConfig) (Just chan) app newGame
    case finishedGame of
         Left msg -> putStrLn msg
         Right gm -> let xs = concatMap showHighScore $ gm ^. T.highscores
                     in  writeFile highScoresFile xs
