{-# OPTIONS_GHC -O2 -Wall               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Gruvbox where

import           Data.Hashable
import qualified Data.Vector   as V
import           XMonad        (Window, X (), runQuery, title)
import           XMonad.Core
import qualified XMonad.Util.ExtensibleState as XS

import Data.Word
import System.Random

backgroundSoft = "#32302f"
backgroundNorm = "#282828"
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

allColors :: [String]
allColors = [ darkRed     , red
            , darkGreen   , green
            , darkYellow  , yellow
            , darkBlue    , blue
            , darkMagenta , magenta
            , darkOrange  , orange
            , darkCyan    , cyan
            , darkWhite   , white
            , gray0
            ]

colorizer :: Window -> Bool -> X (String, String)
colorizer s active
  | active    = return (background, foreground)
  | otherwise = do
    name <- runQuery title s
    let index   = hash name `mod` (length allColors)
        bgcolor = allColors !! index
    return (bgcolor, background)
