--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
-- Usefuly XMonad operations
import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.FindEmptyWorkspace
import           XMonad.Actions.GridSelect
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.SwapWorkspaces
import           XMonad.Actions.UpdatePointer
import           XMonad.Actions.Warp
import           XMonad.Actions.WindowBringer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Util.EZConfig              (additionalKeys)
import           XMonad.Util.Run                   (safeSpawn, spawnPipe)
 -- Layouts
import           XMonad.Layout.AutoMaster
import           XMonad.Layout.Cross
import           XMonad.Layout.Grid                (Grid (..))
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.Minimize
import           XMonad.Layout.NoBorders           (smartBorders)
import           XMonad.Layout.PerWorkspace        (onWorkspace)
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile       (ResizableTall (..))
import           XMonad.Layout.Spiral              (spiral)
import           XMonad.Prompt                     (XPConfig (..),
                                                    defaultXPConfig)
import           XMonad.Prompt.RunOrRaise          (runOrRaisePrompt)
import           XMonad.Prompt.Shell               (shellPrompt)
import           XMonad.Prompt.Window              (windowPromptGoto)
import           XMonad.Util.WindowProperties

-- General libraries
import           Data.Char                         (isAlpha, toLower)
import           Data.Function                     (on)
import           Data.List                         (foldr1, isInfixOf,
                                                    stripPrefix)
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid                       (appEndo)
import           System.Exit
import           System.IO

import qualified Data.Map                          as M
import qualified XMonad.StackSet                   as W


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
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["web", "email", "code"] ++ map show [4..9] ++ ["music"]
-- myWorkspaces    = [ "pheonix", "dragon", "lyorn", "tiassa", "hawk", "dzur"
--                   , "isolla", "tsalmoth", "vallista", "jhereg", "iorich"
--                   , "chreotha", "yendi", "orca", "teckla", "jhegalla", "athyra"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

-- Useful functions for restarting XMonad
xmonadExecutable :: String
xmonadExecutable = "/home/spenser/.cabal/bin/xmonad"

restartXMonad :: X ()
restartXMonad = broadcastMessage ReleaseResources >> restart xmonadExecutable True

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawnHere $ XMonad.terminal conf)
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
    -- Minimize focused window
    , ((modm              , xK_m     ), withFocused minimizeWindow)
    -- Restore next minimized window
    , ((modm .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)
    -- Cycle recent workspaces
    , ((modm              , xK_Tab   ), toggleWS)
    -- Move current window to previous workspace
    , ((modm .|. shiftMask, xK_Tab   ), toggleWindowTo)
    -- Fullscreen apps
    , ((modm, xK_f                   ), fullFloatFocused)
    -- Grid Select Binding
    , ((modm              , xK_g     ), goToSelected defaultGSConfig)
    -- Put cursor in upper left hand corner of the screen
    , ((modm, xK_o                   ), banish UpperLeft)
    -- Find an empty workspace
    , ((modm,               xK_n     ), viewEmptyWorkspace)
    -- Tag current window to an empty workspace and view it
    , ((modm .|. shiftMask, xK_n     ), tagToEmptyWorkspace)
    -- Run XMonad prompt
    , ((modm,               xK_p     ), runOrRaisePrompt infixFindConfig)
    -- Run Window prompt
    , ((modm .|. shiftMask, xK_p     ), windowPromptGoto infixFindConfig)
    -- Next Workspace
    , ((modm,               xK_Right ), nextWS)
    -- Previous Workspace
    , ((modm,               xK_Left  ), prevWS)
    -- Dynamic workspace bindings
    , ((modm .|. shiftMask , xK_BackSpace) , removeWorkspace)
    , ((modm               , xK_v        ) , selectWorkspace defaultXPConfig)
    , ((modm               , xK_b        ) , withWorkspace defaultXPConfig (windows . W.shift))
    , ((modm .|. shiftMask , xK_b        ) , withWorkspace defaultXPConfig (windows . copy))
    , ((modm               , xK_u        ) , renameWorkspace defaultXPConfig)
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
        | (key, n) <- zip workspaceKeys [0..] ]
    ++
    -- Bindings to shift the current window to a different workspace
    [((modm .|. shiftMask, key), withNthWorkspace W.shift n)
        | (key, n) <- zip workspaceKeys [0..] ]

    where -- Keys for specifying workspaces.
          workspaceKeys   = [xK_1 .. xK_9] ++ [xK_0] ++ [xK_F1 .. xK_F12]
          -- Performs an infix search with caseless comparison
          mySearch = isInfixOf `on` map toLower
          -- XPConfig with an infix search, rather than prefix.
          infixFindConfig = defaultXPConfig { searchPredicate = mySearch }
          -- Executes a withNthWorkspace action 's' gased on the key using the
          -- mod key 'm' sending the result to workspace 'n'.
          workspaceAction m s key n = ((m, key), withNthWorkspace s n)

fullFloatFocused =
    withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), \w -> focus w >> windows W.swapMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
grids = GridRatio (4/3) ||| simpleCross ||| autoMaster 1 (1/100) Grid

mainLayouts = tiled ||| mirror ||| Full ||| grids ||| spiral (6/7)
    where tiled  = Tall nmaster delta ratio
          mirror = Mirror tiled
          -- default tiling algorithm partitions the screen into two panes
          -- The default number of windows in the master pane
          nmaster = 1
          -- Default proportion of screen occupied by master pane
          ratio   = 1 / 2
          -- Percent of screen to increment by when resizing panes
          delta   = 3 / 100

myLayout = smartBorders . minimize . avoidStruts $ mainLayouts

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Firefox"        --> doShift (head myWorkspaces)
    , className =? "Thunderbird"    --> doShift (myWorkspaces !! 1)
    , className =? "banshee"        --> doShift (last myWorkspaces)
    , resource  =? "desktop_window" --> doIgnore
    , isFullscreen                  --> doFullFloat ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--

xmobarConfig = xmobarPP { ppTitle  = xmobarColor "green" "" . shorten 75
                        , ppLayout = takeWhile isAlpha . stripper }

myLogHook xmproc = do
    dynamicLogWithPP $ xmobarConfig { ppOutput = hPutStrLn xmproc }
    -- Place pointer in the center of the focused window
    updatePointer (0.5, 0.5) (0, 0)

stripper :: String -> String
stripper g = fromMaybe g $ stripPrefix "Minimize " g

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
main = xmonad . defaults =<< spawnPipe "/home/spenser/.cabal/bin/xmobar"


allHooks = [manageDocks, myManageHook, manageHook defaultConfig, manageSpawn]

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
--
defaults xmproc = defaultConfig
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
    , layoutHook         = myLayout
    , manageHook         = foldr1 (<+>) allHooks
    , logHook            = myLogHook xmproc
    , startupHook        = myStartupHook
    } `additionalKeys`
    [ ((mod4Mask  .|. shiftMask , xK_z  ) , spawn "gnome-screensaver-command --lock" ) ,
      ((mod4Mask                , xK_F1 ) , spawn "firefox"                    ) ,
      ((mod4Mask                , xK_F2 ) , spawn "thunderbird"                ) ,
      ((mod4Mask                , xK_F3 ) , spawnHere "nautilus --no-desktop"  ) ,
      ((mod4Mask                , xK_F4 ) , spawn "gvim"                       ) ,
      ((mod4Mask                , xK_F5 ) , spawn "banshee"                    ) ,
      ((mod4Mask                , xK_F6 ) , spawn "virtualbox"                 ) ,
      ((mod4Mask                , xK_F7 ) , spawn "thunar"                     ) ]

