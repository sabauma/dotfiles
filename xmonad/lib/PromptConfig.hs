module PromptConfig where

import           Data.Char
import           Data.Function           (on)
import           Data.List               (isInfixOf)
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
xmobarTitleColor = "#fb4934"
-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#458588"
-- Color of the layout field
xmobarLayoutColor = "#98971a"

-- XPConfig with an infix search, rather than prefix.
myPromptConfig :: XPConfig
myPromptConfig = def { font = myFont, height = 24, searchPredicate = mySearch }
  where mySearch = isInfixOf `on` map toLower

changeDirPrompt :: X ()
changeDirPrompt = directoryPrompt myPromptConfig "Set working directory: "
                $ \d -> currentWorkspace >>= changeDir d

