{-# LANGUAGE OverloadedStrings #-}
module View
    ( attributes
    , drawUI
    ) where

import qualified Graphics.Vty as V
import qualified Data.Text    as Txt
import qualified Data.Matrix  as M
import qualified Model.Types  as T
import Data.List                        ( foldl'                    )
import Lens.Micro                       ( (.~), (^.), (&)           )
import Brick.Types                      ( Padding (..), Widget (..) )
import Brick.Widgets.Core               ( (<+>), fill, hBox, hLimit
                                        , padLeft, str, txt, vBox
                                        , vLimit, withAttr
                                        , withBorderStyle           )
import Brick.Widgets.Edit               ( editFocusedAttr
                                        , renderEditor              )
import Brick.Widgets.Border.Style       ( unicodeRounded            )
import Brick.Widgets.Border             ( borderAttr
                                        , borderWithLabel           )
import Brick.AttrMap                    ( AttrMap, attrMap          )
import Brick.Util                       ( bg, fg, on                )
import Brick.Widgets.Center             ( center, hCenter, vCenter  )
import Model.Utilities                  ( addHighScore
                                        , highScore
                                        , isFlashing
                                        , playerScore
                                        , powerTimeLeft
                                        , scoreFruit
                                        , tickPeriod                )
import Model.Types                      ( Direction     (..)
                                        , Fruit         (..)
                                        , FruitName     (..)
                                        , Game          (..)
                                        , GameSt        (..)
                                        , Ghost         (..)
                                        , GhostName     (..)
                                        , GhostState    (..)
                                        , HighScore     (..)
                                        , Items         (..)
                                        , Maze          (..)
                                        , Message       (..)
                                        , Mode          (..)
                                        , Name          (..)
                                        , PacMan        (..)
                                        , Point         (..)
                                        , Score         (..)
                                        , Tile          (..)
                                        , Time          (..)        )

-- =============================================================== --
-- Drawing the UI for different game states

drawUI :: GameSt -> [ Widget Name ]
-- ^Entry point for rendering the game state.
drawUI (Left msg) = [ str msg ]
drawUI (Right gm) = case gm ^. T.mode of
                         StartScreen  -> drawStartScreenUI gm
                         GameOver     -> drawGameOverUI gm
                         LevelOver    -> drawLevelOverUI gm
                         NewHighScore -> drawNewHighScoreUI gm
                         ReplayLvl    -> drawReplayUI gm
                         Paused _     -> drawPausedUI gm
                         otherwise    -> drawRunningUI gm

drawRunningUI :: Game -> [ Widget Name ]
-- ^Actual active gameplay UI.
drawRunningUI gm =
    let width = M.ncols $ gm ^. T.maze
        parts = [
                  renderHeader gm
                , renderMaze gm
                , renderFooter gm
                , renderVerticalSpace 1
                , withAttr "controls" . txt $ " Space to pause"
                , withAttr "controls" . txt $ " Esc to quit"
                ]
    in  [ withAttr "background" . center . hLimit width . vBox $ parts ]

drawPausedUI :: Game -> [ Widget Name ]
-- ^Paused gameplay UI.
drawPausedUI gm =
    let width = M.ncols $ gm ^. T.maze
        parts = [
                  renderPausedHeader gm
                , renderMaze gm
                , renderFooter gm
                , renderVerticalSpace 1
                , withAttr "controls" . txt $ " Space to unpause"
                , withAttr "controls" . txt $ " Esc to quit"
                ]
    in  [ withAttr "background" . center . hLimit width . vBox $ parts ]

drawStartScreenUI :: Game -> [ Widget Name ]
-- ^Player has completed a level.
drawStartScreenUI gm =
    let parts = [
                  renderHighScoreDisplay gm
                , renderVerticalSpace 2
                , withAttr "focusControls" . txt $ " Enter to start a new game"
                , withAttr "focusControls" . txt $ " Esc to quit"
                , renderVerticalSpace 1
                , withAttr "controls" . txt $ " Arrow keys change direction"
                , withAttr "controls" . txt $ " Space to pause"
                , withAttr "controls" . txt $ " Esc to quit"
                ]
    in  putInDialogBox "PAC-MAN!" . vBox $ parts

drawLevelOverUI :: Game -> [ Widget Name ]
-- ^Player has completed a level.
drawLevelOverUI gm =
    let title = Txt.pack $ "LEVEL " ++ show (gm ^. T.level) ++ " COMPLETED!"
        parts = [
                  hCenter . renderLabeledScore $ gm
                , renderVerticalSpace 2
                , withAttr "focusControls" . txt $ " Enter to continue"
                , withAttr "focusControls" . txt $ " Esc to quit"
                ]
    in  putInDialogBox title . vBox $ parts

drawReplayUI :: Game -> [ Widget Name ]
-- ^Player has lost a life but still has remaining lives.
drawReplayUI gm =
    let parts = [
                  hCenter . renderLabeledScore $ gm
                , renderVerticalSpace 2
                , withAttr "focusControls" . txt $ " Enter to keep trying"
                , withAttr "focusControls" . txt $ " Esc to quit"
                ]
    in  putInDialogBox "YOU GOT CAPTURED!" . vBox $ parts

drawNewHighScoreUI :: Game -> [ Widget Name ]
-- ^Player has lost all lives and gotten a new high score.
drawNewHighScoreUI gm =
    let parts = [
                  renderHighScoreDisplay gm
                , renderVerticalSpace 1
                , hCenter . withAttr "highScore" . txt $ "Enter your name"
                , hCenter . hLimit 26 . renderEditor (str . unlines) True
                      $ gm ^. T.hsedit
                , renderVerticalSpace 1
                , renderScoreDetails gm
                , renderVerticalSpace 2
                , withAttr "controls" . txt $ " Enter to play again"
                , withAttr "controls" . txt $ " Esc to quit"
                ]
    in  putInDialogBox "NEW HIGH SCORE!" . vBox $ parts

drawGameOverUI :: Game -> [ Widget Name ]
-- ^Player has lost all lives and has not gotten a new high score.
drawGameOverUI gm =
    let parts = [
                  hCenter . withAttr "focusControls" . txt
                      $ " Enter to play again"
                , hCenter . withAttr "focusControls" . txt
                      $ " Esc to quit"
                ]
    in  putInDialogBox "GAME OVER!" . vBox $ parts

-- =============================================================== --
-- Tiling functions for constructing the maze prior to rendering
-- Only fixed maze elements such as walls, warps, pellets and oneways
-- are always part of the maze while variable elements such as the
-- player, ghosts and fruit are managed separately. These tiling
-- functions build a fully tiled maze by incorporating the variable
-- elements just prior to rendering.

---------------------------------------------------------------------
-- Overall maze construction

tileMaze :: Game -> Maze
tileMaze gm = tilePlayer   ( gm ^. T.pacman )
              . tileGhosts   gm
              . tileFruit  ( gm ^. T.fruit  )
              $ gm ^. T.maze

---------------------------------------------------------------------
-- Tiling the fruit

tileFruit :: Maybe Fruit -> Maze -> Maze
tileFruit Nothing m = m
tileFruit (Just frt) m
    | isWaiting = m
    | otherwise = M.setElem frtTile (frt ^. T.fpos) m
    where isWaiting = frt ^. T.fdelay > 0
          frtTile   = FruitTile $ frt ^. T.fname

---------------------------------------------------------------------
-- Tiling the ghosts

tileGhosts :: Game -> Maze -> Maze
tileGhosts gm m0 = foldl' ( \ m (p,t) -> M.setElem t p m) m0 gs
    where gs = [ (g ^. T.gpos, tileGhost gm g) | g <- gm ^. T.ghosts ]

tileGhost :: Game -> Ghost -> Tile
tileGhost gm g = case g ^. T.gstate of
                      Edible    -> tileEdibleGhost gm
                      EyesOnly  -> GhostEyes
                      otherwise -> NormalGhost $ g ^. T.gname

tileEdibleGhost :: Game -> Tile
tileEdibleGhost gm
    | trem >= half = BlueGhost
    | isWhite      = WhiteGhost
    | otherwise    = BlueGhost
    where trem    = powerTimeLeft gm
          half    = quot ( gm ^. T.pwrtime ) 2
          isWhite = isFlashing gm

---------------------------------------------------------------------
-- Tiling the player

tilePlayer :: PacMan -> Maze -> Maze
tilePlayer pm = M.setElem Player (pm ^. T.ppos)

-- =============================================================== --
-- Widget rendering during regular and paused game play

---------------------------------------------------------------------
-- Rendering the maze during normal gameplay

renderMaze :: Game -> Widget Name
-- ^Build and render the maze with fixed and changing tiles in place.
renderMaze gm = vBox . renderTiles . M.toLists . tileMaze $ gm
    where renderTiles = map ( hBox . map (renderTile gm) )

renderTile :: Game -> Tile -> Widget Name
renderTile _ (Wall s)               = withAttr "maze"       . txt $ s
renderTile _ (OneWay North)         = withAttr "oneway"     . txt $ "-"
renderTile _ (OneWay South)         = withAttr "oneway"     . txt $ "-"
renderTile _ (OneWay West )         = withAttr "oneway"     . txt $ "|"
renderTile _ (OneWay East )         = withAttr "oneway"     . txt $ "|"
renderTile _  Pellet                = withAttr "pellet"     . txt $ "."
renderTile gm PwrPellet             = renderPwrPellet gm
renderTile _ (NormalGhost Blinky)   = withAttr "blinky"     . txt $ "\""
renderTile _ (NormalGhost Inky  )   = withAttr "inky"       . txt $ "\""
renderTile _ (NormalGhost Pinky )   = withAttr "pinky"      . txt $ "\""
renderTile _ (NormalGhost Clyde )   = withAttr "clyde"      . txt $ "\""
renderTile _  BlueGhost             = withAttr "blueGhost"  . txt $ "\""
renderTile _  WhiteGhost            = withAttr "whiteGhost" . txt $ "\""
renderTile _  GhostEyes             = withAttr "ghostEyes"  . txt $ "\""
renderTile _ (FruitTile Cherry    ) = withAttr "cherry"     . txt $ "c"
renderTile _ (FruitTile Strawberry) = withAttr "strawberry" . txt $ "s"
renderTile _ (FruitTile Orange    ) = withAttr "orange"     . txt $ "o"
renderTile _ (FruitTile Apple     ) = withAttr "apple"      . txt $ "a"
renderTile _ (FruitTile Melon     ) = withAttr "melon"      . txt $ "m"
renderTile _ (FruitTile Galaxian  ) = withAttr "galaxian"   . txt $ "g"
renderTile _ (FruitTile Bell      ) = withAttr "bell"       . txt $ "b"
renderTile _ (FruitTile Key       ) = withAttr "key"        . txt $ "k"
renderTile gm Player                = renderPlayer gm
renderTile _  _                     = withAttr "maze"       . txt $ " "

renderPlayer :: Game -> Widget Name
renderPlayer g = withAttr "player" . txt . glyph $ g ^. T.pacman . T.pdir
    where glyph North = "∨"
          glyph South = "∧"
          glyph West  = ">"
          glyph East  = "<"

renderPwrPellet :: Game -> Widget Name
renderPwrPellet gm
    | isFlashing gm = withAttr "flashPellet" . txt $ "*"
    | otherwise     = withAttr "pellet"      . txt $ "*"

---------------------------------------------------------------------
-- Rendering scores and messages during regular gameplay
-- These are displayed in a header above the maze during gameplay.

renderScore :: Game -> Widget Name
-- ^Render the player's current score.
renderScore = withAttr "score" . str . show . playerScore

renderHighestScore :: Game -> Widget Name
-- ^Render the highest score recorded, which may be the player's
-- current score.
renderHighestScore gm
    | ps > hs   = withAttr "score" . str . show $ ps
    | otherwise = withAttr "score" . str . show $ hs
    where ps = playerScore gm
          hs = highScore gm

renderMessage :: Game -> Widget Name
-- ^Render messages during regular, unpaused gameplay, such as
-- whether a ghost was captured or a fruit was eaten. If there is
-- no message, then just display the current level.
renderMessage gm =
    let levelMsg = "Level " ++ show ( gm ^. T.level )
     in  case gm ^. T.msg of
              Message s _ -> withAttr "info" . str $ s
              otherwise   -> withAttr "info" . str $ levelMsg

renderHeader :: Game -> Widget Name
-- ^Collect together all the score and message information and
-- display above the maze during regular gameplay.
renderHeader gm = vLimit 3 . vBox $ [ row1, row2, row3 ]
    where row1 = padLeft Max . withAttr "score" . txt $ "High"
          row2 = hBox [ renderMessage gm, hsLabel ]
          row3 = hBox [ renderScore gm, padLeft Max . renderHighestScore $ gm ]
          hsLabel = padLeft Max . withAttr "score" . txt $ "Score"

renderPausedHeader :: Game -> Widget Name
-- ^Same as renderHeader but for paused gameplay.
renderPausedHeader gm = vLimit 3 . vBox $ [ row1, row2, row3 ]
    where row1 = padLeft Max . withAttr "score" . txt $ "High"
          row2 = hBox [ withAttr "info" . txt $ "PAUSED", hsLabel ]
          row3 = hBox [ renderScore gm, padLeft Max . renderHighestScore $ gm ]
          hsLabel = padLeft Max . withAttr "score" . txt $ "Score"

---------------------------------------------------------------------
-- Rendering fruit and oneups
-- These are displayed in a footer during gameplay.

renderFooter :: Game -> Widget Name
-- ^Graphical summary of remaining lives (oneups) and fruit eaten.
renderFooter gm = renderOneups gm <+> ( padLeft Max . renderFruitItems ) gm

renderOneups :: Game -> Widget Name
-- ^Graphical summary of remaining lives.
renderOneups gm
    | n > 0     = hBox . take n . cycle $ [ oneup, blank ]
    | otherwise = blank
    where n     = 2 * gm ^. T.oneups
          oneup = withAttr "player"     . txt $ ">"
          blank = withAttr "background" . txt $ " "

renderFruitItems :: Game -> Widget Name
-- ^Graphical summary of the different types of fruit eaten so far.
renderFruitItems gm = hBox . map ( renderTile gm . FruitTile ) $ names
    where names = fst . unzip $ gm ^. T.items . T.fruits

-- =============================================================== --
-- Rendering the content of dialog boxes
-- Dialog boxes are displayed when there is no gameplay, for example,
-- when the player has just completed a level.

---------------------------------------------------------------------
-- Utilities

putInDialogBox :: Txt.Text -> Widget Name -> [ Widget Name ]
-- ^Render a widget in a bordered box with the specified title.
-- Used for displaying information between game plays.
putInDialogBox title widget =
    let header    = withAttr "info" . txt $ title
        formatted = [
                      renderVerticalSpace 1
                    , widget
                    , renderVerticalSpace 1
                    ]
    in  [
          withAttr "background"
          . center
          . withBorderStyle unicodeRounded
          . borderWithLabel header
          . hLimit 30 . vBox $ formatted
        ]

renderVerticalSpace :: Int -> Widget Name
-- ^Spacer for separating vertically stacked widgets.
renderVerticalSpace n = vLimit n . withAttr "background" . fill $ ' ' 

---------------------------------------------------------------------
-- Rendering score information

renderHighScoreDisplay :: Game -> Widget Name
-- ^Summary of all recorded high scores and players' names.
renderHighScoreDisplay gm =
    let scores = gm ^. T.highscores
        score  = ( "Your name", playerScore gm )
    in  case gm ^. T.mode of
             NewHighScore -> renderHighScores . addHighScore score $ scores
             otherwise    -> renderHighScores scores

renderHighScores :: [HighScore] -> Widget Name
-- ^Helper function for renderHighScoreDisplay that actually displays
-- the scores.
renderHighScores scores =
    let go (name, score) = name ++ " " ++ show score
    in  vBox [
               hCenter . withAttr "highScore" . txt $ "High Scores"
             , vBox . map ( hCenter . withAttr "info" . str . go ) $ scores
             ]

renderLabeledScore :: Game -> Widget Name
-- ^Simple display of score information.
renderLabeledScore gm
    | ps > hs   = hBox [ hsLabel, renderScore gm ]
    | otherwise = hBox [ label,   renderScore gm ]
    where ps      = playerScore gm
          hs      = highScore gm
          hsLabel = withAttr "score" . txt $ "New High Score: "
          label   = withAttr "score" . txt $ "Score: "

renderScoreDetails :: Game -> Widget Name
-- ^Display a breakdown of the player's score.
renderScoreDetails gm =
    vBox [
           hCenter . renderPelletScoreHeader $ gm
         , renderPelletScores $ gm ^. T.items
         , renderVerticalSpace 1
         , hCenter . renderGhostScoreHeader $ gm
         , renderGhostScores  $ gm ^. T.items . T.gstscores
         , renderVerticalSpace 1
         , hCenter renderFruitScoreHeader
         , renderFruitScores  $ gm ^. T.items . T.fruits
         ]

renderPelletScoreHeader :: Game -> Widget Name
-- ^Header for pellet score section when rendering score details.
-- See also renderScoreDetails.
renderPelletScoreHeader gm =
    hBox [
           renderPwrPellet gm
         , withAttr "pelletText" . txt $ " Pellets "
         , renderPwrPellet gm
         ]

renderPelletScores :: Items -> Widget Name
-- ^Summary of score coming from all pellets eaten during gameplay.
-- See also renderScoreDetails.
renderPelletScores x
    | np + npp == 0 = withAttr "info" . txt $ "You didn't get any pellets!"
    | otherwise     = vBox [ go "Regular" np 10, go "Power" npp 50 ]
    where np       = x ^. T.pellets
          npp      = x ^. T.ppellets
          go p n s = hCenter . withAttr "score" . str
                     $ show n ++ " x " ++ p ++ " = " ++ show (n*s)

renderGhostScoreHeader :: Game -> Widget Name
-- ^Header for ghost score section when rendering score details.
-- See also renderScoreDetails.
renderGhostScoreHeader gm =
    hBox [
           renderTile gm . tileEdibleGhost $ gm
         , withAttr "ghostText" . txt $ "  Ghosts  "
         , renderTile gm . tileEdibleGhost $ gm
         ]

renderGhostScores :: [(Score, Int)] -> Widget Name
-- ^Summary of score coming from all ghosts captured during gameplay.
renderGhostScores [] = hCenter . withAttr "info" . txt $ "You didn't catch any!"
renderGhostScores gs = vBox . map (hCenter . withAttr "score" . str . go) $ gs
    where go (x, n) = concat [ show n ++ " x "
                             , show x ++ " = "
                             , show (n * x) ]

renderFruitScoreHeader :: Widget Name
-- ^Header for fruit score section when rendering score details.
-- See also renderScoreDetails.
renderFruitScoreHeader = withAttr "melon" . txt $ "Fruits"

renderFruitScores :: [(FruitName, Int)] -> Widget Name
-- ^Summary of score coming from all fruits eaten during gameplay.
-- See also renderScoreDetails.
renderFruitScores [] = hCenter . withAttr "info" . txt $ "You didn't get any!"
renderFruitScores xs = vBox . map renderFruitScore $ xs
    where renderFruitScore (x, n) =
            let go Cherry    = withAttr "cherry"     . str $ "Cherry"
                go Strawberry= withAttr "strawberry" . str $ "Strawberry"
                go Orange    = withAttr "orange"     . str $ "Orange"
                go Apple     = withAttr "apple"      . str $ "Apple"
                go Melon     = withAttr "melon"      . str $ "Melon"
                go Galaxian  = withAttr "galaxian"   . str $ "Galaxian"
                go Bell      = withAttr "bell"       . str $ "Bell"
                go Key       = withAttr "key"        . str $ "Key"
            in  hCenter . hBox
                $ [
                    withAttr "info" . str $ show n ++ " x "
                  , go x
                  , withAttr "info" . str $ " = " ++ show (n * scoreFruit x)
                  ]

-- =============================================================== --
-- Attributes

attributes :: AttrMap
attributes = attrMap V.defAttr
    [ ( "player",           on V.black  V.brightYellow )
    , ( "maze",             on V.blue          V.black )
    , ( "oneway",           on V.red           V.black )
    , ( "pellet",           on V.white         V.black )
    , ( "flashPellet",      on V.brightBlack   V.black )
    , ( "score",            on V.white         V.black )
    , ( "info",             on V.white         V.black )
    , ( "blinky",           on V.black           V.red )
    , ( "pinky",            on V.black V.brightMagenta )
    , ( "inky",             on V.black    V.brightCyan )
    , ( "clyde",            on V.black        V.yellow )
    , ( "blueGhost",        on V.white          V.blue )
    , ( "whiteGhost",       on V.black         V.white )
    , ( "ghostEyes",        on V.cyan          V.black )
    , ( "cherry",           on V.red           V.black )
    , ( "strawberry",       on V.brightMagenta V.black )
    , ( "orange",           on V.yellow        V.black )
    , ( "apple",            on V.brightRed     V.black )
    , ( "melon",            on V.green         V.black )
    , ( "galaxian",         on V.cyan          V.black )
    , ( "bell",             on V.yellow        V.black )
    , ( "key",              on V.brightYellow  V.black )
    , ( "background",       bg V.black                 )
    , ( "highScore",        on V.yellow        V.black )
    , ( "pelletText",       on V.cyan          V.black )
    , ( "ghostText",        on V.brightBlue    V.black )
    , ( "controls",         on V.brightBlack   V.black )
    , ( "focusControls",    on V.cyan          V.black )
    , ( borderAttr,         on V.blue          V.black )
    , ( editFocusedAttr,    on V.white   V.brightBlack )
    ]
