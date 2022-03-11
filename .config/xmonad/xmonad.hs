-- Base
import System.Exit (exitSuccess)
import XMonad
import qualified XMonad.StackSet as W

-- Actions
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import XMonad.Actions.NoBorders
import XMonad.Actions.TiledWindowDragging
import XMonad.Actions.WithAll (killAll)

-- Hooks
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageHelpers (doFullFloat, doCenterFloat, isFullscreen)
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, manageDocks, Direction2D(D, L, R, U))
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)

-- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral

-- Layouts Modifiers
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull)
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle ((??), EOT (EOT), mkToggle, single)
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing (Spacing, spacingRaw, Border(Border))
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation

-- Data
import Data.Monoid ()
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_AudioPlay, xF86XK_AudioPrev, xF86XK_AudioNext)
import Control.Monad (join, when)

-- Util
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty -e fish"

myBrowser :: String
myBrowser = "firefox"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColor :: String
myNormalBorderColor = "#282828"

myFocusedBorderColor :: String
myFocusedBorderColor = "#d65d0e"

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
-- ["www", "dev", "term", "ref", "git", "dock", "fs", "media", "misc"]
-- [" ", " ", " ", " ", " ", " ", " ", " ", " "]

------------------------------------------------------------------------
-- Key bindings:
--
maimcopy = spawn "maim -s | xclip -selection clipboard -t image/png && notify-send \"Screenshot\" \"Copied to Clipboard\" -i flameshot"
maimsave = spawn "maim ~/Pictures/$(date +%Y-%m-%d_%H-%M-%S).png && notify-send \"Screenshot\" \"Saved to Pictures\" -i flameshot"
rofi = spawn "rofi -no-lazy-grab -show drun -config ~/.config/rofi/config.rasi"

myKeys :: [(String, X ())]
myKeys =
    [
    -- Toggle screens
    ("M-<Rigth>", nextScreen)
    , ("M-<Left>", prevScreen)

    -- Toggle window no borders
    , ("M-g", withFocused toggleBorder)
    
    -- Multitoggle
    , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)

    -- Launch a terminal
    , ("M-<Return>", spawn myTerminal)

    -- Launch browser
    , ("M-b", spawn myBrowser)

    -- Launch rofi and dashboard
    , ("M-o", rofi)
    , ("M-p", spawn "exec $HOME/.config/eww/launch")

    -- Audio keys
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q -D pulse set Master 5%+ unmute")
    , ("<XF86AudioLowerVolume>", spawn "amixer -q -D pulse set Master 5%- unmute")
    , ("<XF86AudioMute>", spawn "amixer -q -D pulse set Master toggle")

    -- Brightness keys
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl s +10%")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 10-%")
 
    -- Screenshot
    , ("M-S-s", maimcopy)
    , ("<Print>", maimsave)

    -- Close windows
    , ("M-S-c", kill1)
    , ("M-S-a", killAll)

     -- Rotate through the available layouts
    , ("M-<Space>", sendMessage NextLayout)

    -- Move focus between windows
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)

    -- Move focus to the master window
    , ("M-m", windows W.focusMaster)

    -- Move the focused window to the master window
    , ("M-S-m", windows W.shiftMaster)

    -- Swap the focused window
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)

    -- Shrink or expand windows
    , ("M-S-h", sendMessage Shrink)
    , ("M-S-l", sendMessage Expand)
    , ("C-S-j", sendMessage MirrorShrink)
    , ("C-S-k", sendMessage MirrorExpand)

    -- Push window back into tiling
    , ("M-S-<Return>", withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ("M-.", sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN (-1)))

    -- XMonad
    , ("M-S-r", spawn "xmonad --recompile")
    , ("M-C-r", spawn "xmonad --restart")
    , ("M-S-q", io exitSuccess)

    ]
    ++

    -- mod-[1..9] @@ Switch to workspace N
    -- mod-shift-[1..9] @@ Move client to workspace N
    -- mod-control-shift-[1..9] @@ Copy client to workspace N
    [(m ++ "M-" ++ key, action tag)
        | (tag, key) <- zip myWorkspaces (map (\x -> "<F" ++ show x ++ ">") [1..9])
        , (m, action) <- [("", windows . W.greedyView), ("S-", windows . W.shift)]]

------------------------------------------------------------------------
-- Mouse bindings:
--
myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

------------------------------------------------------------------------
-- Layouts:

-- Spacing
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a 
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Single window with no gaps
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a 
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall    = renamed [Replace "tall"]
          $ limitWindows 10
          $ mySpacing 6
          $ ResizableTall 1 (3/100) (1/2) []

magnif = renamed [Replace "magnif"]
          $ magnifier
          $ limitWindows 10
          $ mySpacing 6
          $ ResizableTall 1 (3/100) (1/2) []

monocle = renamed [Replace "monocle"]
          $ mySpacing 6 
          $ limitWindows 10 
          $ noBorders Full

grid    = renamed [Replace "grid"]
          $ limitWindows 12
          $ mySpacing 6
          $ Grid (16/10)

spirals = renamed [Replace "spirals"]
          $ mySpacing 6
          $ spiral (6/7)

myLayoutHook = avoidStruts
    $ smartBorders
    $ mouseResize
    $ windowArrange
    $ windowNavigation
    $ draggingVisualizer
    $ mkToggle (NOBORDERS ?? NBFULL ?? EOT) myDefaultLayout
    where
      myDefaultLayout = tall ||| spirals ||| magnif ||| grid ||| monocle 

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
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
    , className =? "zoom" --> doFloat
    , isFullscreen --> doFullFloat
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = fullscreenEventHook <+> fadeWindowsEventHook


------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = fadeWindowsLogHook myFadeHook
-- myLogHook = return ()

myFadeHook = composeAll [                 opaque
                        , isUnfocused --> transparency 0.3
                        ]
------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    -- spawnOnce "lxsession"
    spawnOnce "polybar --config=~/.config/polybar/config.ini main"
    spawnOnce "feh --bg-scale ~/Pictures/Wallpapers/solo-level.png"
    spawnOnce "picom --experimental-backends"
    spawnOnce "greenclip daemon"
    spawnOnce "dunst"
    setWMName "LG3D"

------------------------------------------------------------------------
main :: IO ()
main = xmonad 
    $ fullscreenSupport 
    $ docks 
    $ ewmh 
    $ def
    {
        -- Simple stuff
        modMask            = myModMask,
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        workspaces         = myWorkspaces,

        -- Hooks
        layoutHook         = myLayoutHook,
        manageHook         = insertPosition Below Newer <+> myManageHook, 
        handleEventHook    = myEventHook,
        logHook            = myLogHook >> workspaceHistoryHook,
        startupHook        = myStartupHook
    } `additionalKeysP` myKeys
