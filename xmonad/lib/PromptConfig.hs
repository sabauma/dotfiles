module PromptConfig where

import           Data.Char
import           Data.Function           (on)
import           Data.List               (isInfixOf)
import           XMonad
import           XMonad.Prompt
import           XMonad.Prompt.Directory (directoryPrompt)

import           PerWorkspaceDirs        (changeDir, currentWorkspace)

myFont :: String
myFont = "xft:Droid Sans Mono:size=12"

xmobarTitleColor, xmobarCurrentWorkspaceColor :: String
-- Color of current window title in xmobar.
xmobarTitleColor = "#FFA6A0"
-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#8EFF6C"

-- XPConfig with an infix search, rather than prefix.
myPromptConfig :: XPConfig
myPromptConfig = def { font = myFont, height = 24, searchPredicate = mySearch }
  where mySearch = isInfixOf `on` map toLower

changeDirPrompt :: X ()
changeDirPrompt = directoryPrompt myPromptConfig "Set working directory: "
                $ \d -> currentWorkspace >>= changeDir d

