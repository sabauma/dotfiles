{-# OPTIONS_GHC -O2 -Wall #-}

module Gruvbox where

import           Data.Hashable
import qualified Data.Vector   as V
import           XMonad        (Window, X (), runQuery, title)

backgroundSoft = "#282828"
backgroundHard = "#1d2021"
foreground     = "#ebdbb2"
background     = backgroundHard

gray0        = "#928374"
gray1        = "#a89984"
black        = "#282828"
bBlack       = "#928374"
darkRed      = "#cc241d"
red          = "#fb4934"
darkGreen    = "#98971a"
green        = "#b8bb26"
darkYellow   = "#d79921"
yellow       = "#fabd2f"
darkBlue     = "#458588"
blue         = "#83a598"
darkMagenta  = "#b16286"
magenta      = "#d3869b"
darkCyan     = "#689d6a"
cyan         = "#8ec07c"
darkOrange   = "#d65d0e"
orange       = "#fe8019"
darkWhite    = "#a89984"
white        = "#ebdbb2"

-- Alternatives from the light variant of the color scheme
gray2        = "#3c3836"
darkRed'     = "#9d0006"
darkGreen'   = "#79740e"
darkYellow'  = "#b57614"
darkBlue'    = "#076678"
darkMagenta' = "#8f3f71"
darkCyan'    = "#427b58"
darkOrange'  = "#af3a03"
darkWhite'   = "#7c6f64"

allColors :: V.Vector String
allColors = V.fromList [ darkRed     , darkRed'     , red
                       , darkGreen   , darkGreen'   , green
                       , darkYellow  , darkYellow'  , yellow
                       , darkBlue    , darkBlue'    , blue
                       , darkMagenta , darkMagenta' , magenta
                       , darkOrange  , darkOrange'  , orange
                       , darkCyan    , darkCyan'    , cyan
                       , darkWhite   , darkWhite'   , white
                       , gray0       , gray1        , gray2
                       ]

colorizer :: Window -> Bool -> X (String, String)
colorizer s active
  | active    = return (background, foreground)
  | otherwise = do
    name <- runQuery title s
    let index   = hash name `mod` (V.length allColors)
        bgcolor = V.unsafeIndex allColors index
    return (bgcolor, background)

