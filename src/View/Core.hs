{-# LANGUAGE OverloadedStrings #-}

module View.Core
    ( attributes
    , putInDialogBox
    , renderVerticalSpace
    ) where

import qualified Graphics.Vty as V
import qualified Data.Text    as Txt
import Brick.Types                      ( Widget (..)               )
import Brick.AttrMap                    ( AttrMap, attrMap          )
import Brick.Util                       ( bg, fg, on                )
import Brick.Widgets.Border             ( borderAttr
                                        , borderWithLabel           )
import Brick.Widgets.Core               ( fill, hLimit, txt, vBox
                                        , vLimit, withAttr
                                        , withBorderStyle           )
import Brick.Widgets.Center             ( center                    )
import Brick.Widgets.Border.Style       ( unicodeRounded            )
import Brick.Widgets.Edit               ( editFocusedAttr           )
import Model.Types                      ( Name (..)                 )

---------------------------------------------------------------------
---------------------------------------------------------------------
-- Core attributes and utilities for drawing widgets

-- =============================================================== --
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

-- =============================================================== --
-- Attributes

attributes :: AttrMap
attributes = attrMap V.defAttr
    [ ( "player",           on V.black  V.brightYellow )
    , ( "blueMaze",         on V.blue          V.black )
    , ( "pinkMaze",         on V.magenta       V.black )
    , ( "cyanMaze",         on V.cyan          V.black )
    , ( "redMmaze",         on V.red           V.black )
    , ( "whiteMaze",        on V.white         V.black )
    , ( "deathMaze",        on V.brightBlack   V.black )
    , ( "oneway",           on V.red           V.black )
    , ( "pellet",           on V.white         V.black )
    , ( "pwrPellet",        on V.cyan          V.black )
    , ( "flashPwrPellet",   on V.brightCyan    V.black )
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
