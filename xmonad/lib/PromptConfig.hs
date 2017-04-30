{-# OPTIONS_GHC -O2 -Wall #-}
module PromptConfig where

import           Data.Char
import           Data.Function           (on)
import           Data.List               (isPrefixOf)
import           Gruvbox                 as Colors
import           Text.Printf
import           XMonad
import           XMonad.Prompt
import           XMonad.Prompt.Directory (directoryPrompt)

import           PerWorkspaceDirs        (changeDir, currentWorkspace)

myFont :: Int -> String
myFont = printf "xft:Fira Mono:size=%d"

-- These color were taken from the the gruvbox color scheme.
-- See the .Xresources file for more color information.
xmobarTitleColor, xmobarCurrentWorkspaceColor, xmobarLayoutColor :: String
-- Color of current window title in xmobar.
xmobarTitleColor = Colors.darkMagenta
-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = Colors.darkBlue
-- Color of the layout field
xmobarLayoutColor = Colors.yellow

backgroundColor = Colors.background
foregroundColor = Colors.foreground



-- XPConfig with an infix search, rather than prefix.
myPromptConfig :: XPConfig
myPromptConfig = def { bgColor         = backgroundColor
                     , fgColor         = foregroundColor
                     , bgHLight        = backgroundColor
                     , fgHLight        = Colors.yellow
                     , alwaysHighlight = True
                     , font            = myFont 12
                     , height          = 24
                     , searchPredicate = mySearch }
  where mySearch = isPrefixOf `on` map toLower

changeDirPrompt :: X ()
changeDirPrompt = directoryPrompt myPromptConfig "Set working directory: " setWorkspace
  where setWorkspace d = currentWorkspace >>= changeDir d

