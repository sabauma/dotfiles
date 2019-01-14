{-# OPTIONS_GHC -O2 -Wall #-}
module PromptConfig where

import           Data.Char
import           Data.Function           (on)
import           Data.List               (isInfixOf)
import           Gruvbox                 as Colors
import           XMonad
import           XMonad.Prompt
import           XMonad.Prompt.Directory (directoryPrompt)
import           XMonad.Prompt.FuzzyMatch

import           PerWorkspaceDirs        (changeDir, currentWorkspace)

myFont :: Int -> String
myFont size = "xft:Fira Mono:size=" ++ show size

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
                     , fgColor         = Colors.blue
                     , bgHLight        = Colors.blue
                     , fgHLight        = backgroundColor
                     , alwaysHighlight = False
                     , font            = myFont 16
                     , height          = 36
                     , searchPredicate = fuzzyMatch
                     , sorter          = fuzzySort }

-- A variant of myPromptConfig which always highlights
autoPromptConfig :: XPConfig
autoPromptConfig = myPromptConfig { alwaysHighlight = True }

changeDirPrompt :: X ()
changeDirPrompt = directoryPrompt myPromptConfig "Set working directory: " setWorkspace
  where setWorkspace d = currentWorkspace >>= changeDir d

