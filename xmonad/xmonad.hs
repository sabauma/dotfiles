{-# OPTIONS_GHC -O2 -Wall #-}
module Main where

import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.GridSelect
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.SwapWorkspaces
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.Warp
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Util.EZConfig             (additionalKeys)
import           XMonad.Util.Run                  (spawnPipe)
-- Layouts
import           XMonad.Layout.Grid               (Grid (..))
import           XMonad.Layout.NoBorders          (smartBorders)

import           XMonad.Layout.Spacing            (smartSpacing)
import           XMonad.Prompt.RunOrRaise         (runOrRaisePrompt)
import           XMonad.Prompt.Window             (windowPromptGoto)

-- General libraries
import           Data.Char
import           Data.List                        ((\\))
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      (appEndo)
import           FindEmptyWorkspace
import           Gruvbox                          as Colors
import           PerWorkspaceDirs                 (currentWorkspace, getDir)
import           PromptConfig
import           System.Exit
import           System.IO
import           Text.Printf                      (printf)

import           Graphics.X11.ExtraTypes.XF86

import qualified Data.Map                         as M
import qualified XMonad.StackSet                  as W


-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "gnome-terminal"


-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod1Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
myWorkspaces :: [String]
myWorkspaces = ["1:web", "2:email", "3:code"] ++ map show [4..9] ++ ["10:music", "11:im", "12:torrents"]

-- Border colors for unfocused and focused windows, respectively.
-- Based off of the gruvbox color scheme
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor  = Colors.foreground
myFocusedBorderColor = Colors.darkRed

-- Useful functions for restarting XMonad
xmonadExecutable :: String
xmonadExecutable = "/home/spenser/.cabal/bin/xmonad"

restartXMonad :: X ()
restartXMonad = broadcastMessage ReleaseResources >> restart xmonadExecutable True

spawnInCurDir :: String -> X ()
spawnInCurDir c = currentWorkspace >>= getDir >>= spawnInDir c
  where
    spawnInDir :: String -> String -> X ()
    spawnInDir command s = spawnHere $ printf "cd %s ; %s" s command

gridSelectConfig :: GSConfig Window
gridSelectConfig = def {gs_font=myFont 10, gs_colorizer=Colors.colorizer}

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawnInCurDir $ XMonad.terminal conf)
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill1)
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io exitSuccess)
    -- Restart xmonad
    , ((modm              , xK_q     ), restartXMonad)
    -- Cycle recent workspaces
    , ((modm              , xK_Tab   ), toggleWS)
    -- Move current window to previous workspace
    -- , ((modm .|. shiftMask, xK_Tab   ), toggleWindowTo)
    -- Fullscreen apps
    , ((modm, xK_f                   ), fullFloatFocused)
    -- Grid Select Binding
    , ((modm              , xK_g     ), goToSelected gridSelectConfig)
    -- Put cursor in upper left hand corner of the screen
    , ((modm, xK_o                   ), banish UpperLeft)
    -- Find an empty workspace
    , ((modm,               xK_n     ), viewEmptyWorkspace)
    -- Tag current window to an empty workspace and view it
    , ((modm .|. shiftMask, xK_n     ), tagToEmptyWorkspace)
    -- Run XMonad prompt
    , ((modm,               xK_p     ), spawn "rofi -show run")
    {-, ((modm,               xK_p     ), runOrRaisePrompt myPromptConfig)-}
    -- Run Window prompt
    , ((modm .|. shiftMask, xK_p     ), windowPromptGoto myPromptConfig)
    -- Next Workspace
    , ((modm,               xK_Right ), nextWS)
    -- Previous Workspace
    , ((modm,               xK_Left  ), prevWS)
    -- Dynamic workspace bindings
    , ((modm .|. shiftMask , xK_BackSpace) , removeWorkspace)
    , ((modm               , xK_v        ) , selectWorkspace myPromptConfig)
    , ((modm               , xK_b        ) , withWorkspace myPromptConfig (windows . W.shift))
    , ((modm .|. shiftMask , xK_b        ) , withWorkspace myPromptConfig (windows . copy))
    -- Two dimensional navigation
    , ((mod4Mask , xK_l) , windowGo R True)
    , ((mod4Mask , xK_h) , windowGo L True)
    , ((mod4Mask , xK_k) , windowGo U True)
    , ((mod4Mask , xK_j) , windowGo D True)
    -- Two dimensional swapping
    , ((mod4Mask .|. shiftMask , xK_l) , windowSwap R True)
    , ((mod4Mask .|. shiftMask , xK_h) , windowSwap L True)
    , ((mod4Mask .|. shiftMask , xK_k) , windowSwap U True)
    , ((mod4Mask .|. shiftMask , xK_j) , windowSwap D True)

    -- Move focus between screens
    , ((modm               , xK_w) , prevScreen)
    , ((modm               , xK_r) , nextScreen)
    , ((modm .|. shiftMask , xK_w) , shiftPrevScreen)
    , ((modm .|. shiftMask , xK_r) , shiftNextScreen)

    , ((noModMask , xF86XK_AudioLowerVolume) , amixer "set Master 2- -c 1")
    , ((noModMask , xF86XK_AudioRaiseVolume) , amixer "set Master 2+ -c 1")
    , ((noModMask , xF86XK_AudioMute)        , amixer "set Master toggle -c 1")
    , ((noModMask , xF86XK_MonBrightnessDown), backlight "-dec 10")
    , ((noModMask , xF86XK_MonBrightnessUp)  , backlight "-inc 10")
    -- Set working directory for a workspace
    , ((modm      , xK_d) , changeDirPrompt)
    ]
    ++
    --
    -- add bindings to swap workspaces
    -- swaps workspaces using mod+ctrl+n
    [((modm .|. controlMask, k), windows $ swapWithCurrent i)
        | (i, k) <- zip myWorkspaces workspaceKeys]
    ++
    -- Bindings to shift the view to workspaces using the workspace keys
    [((modm, key), withNthWorkspace W.greedyView n)
        | (key, n) <- zip workspaceKeys [0 ..] ]
    ++
    -- Bindings to shift the current window to a different workspace
    [((modm .|. shiftMask, key), withNthWorkspace W.shift n)
        | (key, n) <- zip workspaceKeys [0 ..] ]

    where -- Keys for specifying workspaces.
          workspaceKeys   = [xK_1 .. xK_9] ++ [xK_0] ++ [xK_F1 .. xK_F12]

          amixer :: (MonadIO m) => String -> m ()
          amixer = spawn . printf "amixer %s"

          backlight :: (MonadIO m) => String -> m ()
          backlight = spawn . printf "xbacklight %s"

fullFloatFocused =
    withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), \w -> focus w >> windows W.swapMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w)
    ]

------------------------------------------------------------------------
-- Layouts:

mainLayouts = smartSpacing 5 $ smartBorders $ avoidStruts $ tiled ||| mirror ||| grid ||| Full
  where
    tiled  = Tall nmaster delta ratio
    mirror = Mirror tiled
    grid   = GridRatio (4 / 3)
    -- default tiling algorithm partitions the screen into two panes
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1 / 2
    -- Percent of screen to increment by when resizing panes
    delta   = 3 / 100

------------------------------------------------------------------------
-- Window rules

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "desktop"        --> doIgnore
    , className =? "Firefox"        --> doShift "1:web"
    , isFullscreen                  --> doFullFloat ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

------------------------------------------------------------------------
-- Status bars and logging

isJunk :: String -> Bool
isJunk x = x == "SmartSpacing" || any isNumber x

cleanupLayout :: String -> String
cleanupLayout s = foldr const s $ filter (not . isJunk) ss
  where ss = words s

xmobarConfig = xmobarPP
             { ppTitle   = title
             , ppLayout  = layout
             , ppCurrent = current
             , ppSep     = sep }
  where
    title   = xmobarColor xmobarTitleColor "" . shorten 100
    layout  = xmobarColor xmobarLayoutColor "" . cleanupLayout
    current = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "«" "»"
    sep     = "   "

myLogHook xmproc = do
    dynamicLogWithPP $ xmobarConfig { ppOutput = hPutStrLn xmproc }
    -- Place pointer in the center of the focused window
    updatePointer (0.5, 0.5) (0, 0)

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = xmonad . defaults =<< spawnPipe "/home/spenser/.cabal/bin/xmobar"

allHooks :: [ManageHook]
allHooks = [manageDocks, myManageHook, manageHook def, manageSpawn]

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
--
defaults xmproc = docks $ def
    { -- Simple Stuff
      terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
      -- numlockMask        = myNumlockMask,
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor

      -- key bindings
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
      -- hooks, layouts
    , layoutHook         = mainLayouts
    , manageHook         = foldr1 (<+>) allHooks
    , logHook            = myLogHook xmproc
    , startupHook        = myStartupHook
    , handleEventHook    = handleEventHook def <+> fullscreenEventHook
    } `additionalKeys`
    [ ((mod4Mask  .|. shiftMask , xK_z  ) , spawn "gnome-screensaver-command --lock" ) ,
      ((mod4Mask                , xK_F1 ) , spawn "firefox"                          ) ,
      ((mod4Mask                , xK_F3 ) , spawnInCurDir "nautilus --no-desktop ."  ) ]

