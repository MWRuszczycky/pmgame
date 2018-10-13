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
import Brick.Widgets.Edit               ( renderEditor              )
import Brick.Widgets.Border.Style       ( unicodeRounded            )
import Brick.Widgets.Border             ( borderAttr
                                        , borderWithLabel           )
import Brick.AttrMap                    ( AttrMap, attrMap          )
import Brick.Util                       ( bg, fg, on                )
import Brick.Widgets.Center             ( center, hCenter, vCenter  )
import Model.Utilities                  ( highScore
                                        , isFlashing
                                        , playerScore
                                        , powerTimeLeft
                                        , tickPeriod                )
import Model.Types                      ( Direction     (..)
                                        , Fruit         (..)
                                        , FruitName     (..)
                                        , Game          (..)
                                        , GameSt        (..)
                                        , Ghost         (..)
                                        , GhostName     (..)
                                        , GhostState    (..)
                                        , Maze          (..)
                                        , Message       (..)
                                        , Mode          (..)
                                        , Name          (..)
                                        , PacMan        (..)
                                        , Point         (..)
                                        , Tile          (..)
                                        , Time          (..)        )

-- =============================================================== --
-- Drawing the UI for different game states

drawUI :: GameSt -> [ Widget Name ]
drawUI (Left msg) = [ str msg ]
drawUI (Right gm) = case gm ^. T.mode of
                         GameOver     -> drawGameOverUI gm
                         LevelOver    -> drawLevelOverUI gm
                         NewHighScore -> drawNewHighScoreUI gm
                         ReplayLvl    -> drawReplayUI gm
                         Paused _     -> drawPausedUI gm
                         otherwise    -> drawRunningUI gm

drawRunningUI :: Game -> [ Widget Name ]
-- ^Actual active gameplay UI.
drawRunningUI gm = [ withAttr "background" ui ]
    where ui = center . hLimit ( M.ncols $ gm ^. T.maze ) . vBox $ ws
          ws = [ renderHeader gm
               , renderMaze gm
               , renderOneups gm <+> renderFruitItems gm
               ]

drawPausedUI :: Game -> [ Widget Name ]
-- ^Paused gameplay UI.
drawPausedUI gm = [ withAttr "background" ui ]
    where ui = center . hLimit ( M.ncols $ gm ^. T.maze ) . vBox $ ws
          ws = [ renderPausedHeader gm
               , renderMaze gm
               , renderOneups gm <+> renderFruitItems gm
               ]

drawLevelOverUI :: Game -> [ Widget Name ]
-- ^Player has completed a level.
drawLevelOverUI gm = [ withAttr "background" ui ]
    where hdrTxt = "LEVEL " ++ show (gm ^. T.level) ++ " COMPLETED!"
          hdr = withAttr "info" . str $ hdrTxt
          ui  = center
                . withBorderStyle unicodeRounded
                . borderWithLabel hdr
                . hLimit 30
                . vLimit 5
                . center
                . vBox $ [ renderLabeledScore gm
                         , withAttr "info" . txt $ "Enter to continue"
                         , withAttr "info" . txt $ "Esc to quit"
                         ]

drawReplayUI :: Game -> [ Widget Name ]
-- ^Player has lost a life but still has remaining lives.
drawReplayUI gm = [ withAttr "background" ui ]
    where hdr = withAttr "info" . txt $ "YOU GOT CAPTURED!"
          ui  = center
                . withBorderStyle unicodeRounded
                . borderWithLabel hdr
                . hLimit 30
                . vLimit 5
                . center
                . vBox $ [ renderLabeledScore gm
                         , withAttr "info" . txt $ "Enter to keep trying"
                         , withAttr "info" . txt $ "Esc to quit"
                         ]

drawNewHighScoreUI :: Game -> [ Widget Name ]
-- ^Player has lost all lives and gotten a new high score.
drawNewHighScoreUI gm = [ withAttr "background" ui ]
    where hdr = withAttr "info" . txt $ "NEW HIGH SCORE!"
          ui  = center
                . withBorderStyle unicodeRounded
                . borderWithLabel hdr
                . hLimit 30
                . vLimit 5
                . center
                . vBox $ [ renderLabeledScore gm
                         , withAttr "info" . txt $ "Enter to play again"
                         , withAttr "info" . txt $ "Esc to quit"
                         , renderEditor (str . unlines) True $ gm ^. T.hsedit
                         ]

drawGameOverUI :: Game -> [ Widget Name ]
-- ^Player has lost all lives and has not gotten a new high score.
drawGameOverUI gm = [ withAttr "background" ui ]
    where hdr = withAttr "info" . txt $ "GAME OVER!"
          ui  = center
                . withBorderStyle unicodeRounded
                . borderWithLabel hdr
                . hLimit 30
                . vLimit 5
                . center
                . vBox $ [ renderLabeledScore gm
                         , withAttr "info" . txt $ "Enter to play again"
                         , withAttr "info" . txt $ "Esc to quit"
                         ]

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
-- Widget rendering

---------------------------------------------------------------------
-- Rendering the maze

renderMaze :: Game -> Widget Name
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
-- Rendering scores and messages

renderHighScore :: Game -> Widget Name
renderHighScore gm
    | ps > hs   = withAttr "score" . str . show $ ps
    | otherwise = withAttr "score" . str . show $ hs
    where ps = playerScore gm
          hs = highScore gm

renderScore :: Game -> Widget Name
renderScore = withAttr "score" . str . show . playerScore

renderLabeledScore :: Game -> Widget Name
renderLabeledScore gm
    | ps > hs   = hBox [ hsLabel, renderScore gm ]
    | otherwise = hBox [ label,   renderScore gm ]
    where ps      = playerScore gm
          hs      = highScore gm
          hsLabel = withAttr "score" . txt $ "New High Score: "
          label   = withAttr "score" . txt $ "Score: "

renderMessage :: Game -> Widget Name
renderMessage gm = let levelMsg = "Level " ++ show ( gm ^. T.level )
                   in  case gm ^. T.msg of
                            Message s _ -> withAttr "info" . str $ s
                            otherwise   -> withAttr "info" . str $ levelMsg

---------------------------------------------------------------------
-- Rendering score information and messages in header

renderHeader :: Game -> Widget Name
renderHeader gm = vLimit 3 . vBox $ [ row1, row2, row3 ]
    where row1    = padLeft Max . withAttr "score" . txt $ "High"
          row2    = hBox [ renderMessage gm, hsLabel ]
          row3    = hBox [ renderScore gm, padLeft Max . renderHighScore $ gm ]
          hsLabel = padLeft Max . withAttr "score" . txt $ "Score"

renderPausedHeader :: Game -> Widget Name
renderPausedHeader gm = vLimit 3 . vBox $ [ row1, row2, row3 ]
    where row1    = padLeft Max . withAttr "score" . txt $ "High"
          row2    = hBox [ withAttr "info" . txt $ "PAUSED", hsLabel ]
          row3    = hBox [ renderScore gm, padLeft Max . renderHighScore $ gm ]
          hsLabel = padLeft Max . withAttr "score" . txt $ "Score"

---------------------------------------------------------------------
-- Rendering fruit and oneups

renderOneups :: Game -> Widget Name
renderOneups gm = hBox . take (2 * gm ^. T.oneups) . cycle $ [ oneup, space ]
    where oneup = withAttr "player"     . txt $ ">"
          space = withAttr "background" . txt $ " "

renderFruitItems :: Game -> Widget Name
renderFruitItems gm = padLeft Max
                      . hBox
                      . map ( renderTile gm . FruitTile )
                      . fst
                      . unzip
                      $ gm ^. T.items . T.fruits

-- =============================================================== --
-- Attributes

attributes :: AttrMap
attributes = attrMap V.defAttr
    [ ( "player",      on V.black V.brightYellow  )
    , ( "maze",        on V.blue V.black          )
    , ( "oneway",      on V.red V.black           )
    , ( "pellet",      on V.white V.black         )
    , ( "flashPellet", on V.brightBlack V.black   )
    , ( "score",       on V.white V.black         )
    , ( "info",        on V.white V.black         )
    , ( "blinky",      on V.black V.red           )
    , ( "pinky",       on V.black V.brightMagenta )
    , ( "inky",        on V.black V.brightCyan    )
    , ( "clyde",       on V.black V.yellow        )
    , ( "blueGhost",   on V.white V.blue          )
    , ( "whiteGhost",  on V.black V.white         )
    , ( "ghostEyes",   on V.cyan V.black          )
    , ( "cherry",      on V.red V.black           )
    , ( "strawberry",  on V.brightMagenta V.black )
    , ( "orange",      on V.yellow V.black        )
    , ( "apple",       on V.brightRed V.black     )
    , ( "melon",       on V.green V.black         )
    , ( "galaxian",    on V.cyan V.black          )
    , ( "bell",        on V.yellow V.black        )
    , ( "key",         on V.brightYellow V.black  )
    , ( "background",  bg V.black                 )
    , ( borderAttr,    on V.blue V.black          )
    ]
