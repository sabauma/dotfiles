{-# OPTIONS_GHC -O2 #-}
module PromptConfig where

import           Data.Char
import           Data.Function           (on)
import           Data.List               (isInfixOf)
import           Gruvbox                 as Colors
import           XMonad
import           XMonad.Prompt
import           XMonad.Prompt.Directory (directoryPrompt)

import           PerWorkspaceDirs        (changeDir, currentWorkspace)

myFont :: String
myFont = "xft:Fira Mono:size=12"

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
myPromptConfig = def { bgColor = backgroundColor
                     , fgColor = foregroundColor
                     , bgHLight = backgroundColor
                     , fgHLight = Colors.darkMagenta
                     , font = myFont
                     , height = 24
                     , searchPredicate = mySearch }
  where mySearch = isInfixOf `on` map toLower

changeDirPrompt :: X ()
changeDirPrompt = directoryPrompt myPromptConfig "Set working directory: "
                $ \d -> currentWorkspace >>= changeDir d

