
module Gruvbox where

import           Data.Hashable
import qualified Data.Vector   as V
import           XMonad        (X ())

background  = "#1d2021"
foreground  = "#ebdbb2"
black       = "#282828"
bBlack      = "#928374"
darkRed     = "#cc241d"
red         = "#fb4934"
darkGreen   = "#98971a"
green       = "#b8bb26"
darkYellow  = "#d79921"
yellow      = "#fabd2f"
darkBlue    = "#458588"
blue        = "#83a598"
darkMagenta = "#b16286"
magenta     = "#d3869b"
darkCyan    = "#689d6a"
cyan        = "#8ec07c"
darkOrange  = "#d65d0e"
orange      = "#fe8019"
darkWhite   = "#a89984"
white       = "#ebdbb2"

allColors :: V.Vector String
allColors = V.fromList [ darkRed     , red
                       , darkGreen   , green
                       , darkYellow  , yellow
                       , darkBlue    , blue
                       , darkMagenta , magenta
                       , darkOrange  , orange
                       , darkCyan    , cyan ]

numColors :: Int
numColors = V.length allColors

colorizer :: (Hashable a, Monad m) => a -> Bool -> m (String, String)
colorizer s active
  | active    = return (background, foreground)
  | otherwise = return (bgcolor   , background)
  where
    bgcolor = allColors V.! (hash s `rem` numColors)

