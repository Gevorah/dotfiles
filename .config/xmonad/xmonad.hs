-- Base
import System.Exit (exitSuccess)
import XMonad
import qualified XMonad.StackSet as W

-- Actions
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseResize
import XMonad.Actions.NoBorders
import XMonad.Actions.TiledWindowDragging

-- Hooks
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Hooks.ManageDocks (ToggleStruts (..), avoidStruts, docks, manageDocks, Direction2D(D, L, R, U))
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)

-- Layouts
import XMonad.Layout.Circle
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull)
import XMonad.Layout.Gaps
    (Direction2D(D, L, R, U),
    gaps,
    setGaps,
    GapMessage(DecGap, ToggleGaps, IncGap))
import XMonad.Layout.MultiToggle ((??), EOT (EOT), mkToggle, single)
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing (spacingRaw, Border(Border))
import qualified XMonad.Layout.ToggleLayouts as T (ToggleLayout (Toggle), toggleLayouts)
import XMonad.Layout.WindowArranger

-- Data
import Data.Monoid ()
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_AudioPlay, xF86XK_AudioPrev, xF86XK_AudioNext)
import Control.Monad (join, when)

-- Util
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)

myTerminal = "alacritty" :: String

myFocusFollowsMouse = True :: Bool
myClickJustFocuses = False :: Bool

myBorderWidth = 1 :: Dimension

myModMask = mod4Mask :: KeyMask

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
-- ["www", "dev", "term", "ref", "git", "dock", "fs", "media", "misc"]
-- [" ", " ", " ", " ", " ", " ", " ", " ", " "]

myNormalBorderColor = "#282828" :: String
myFocusedBorderColor = "#d65d0e" :: String

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]
------------------------------------------------------------------------
-- Key bindings:
--
--center_control = spawn "exec ~/.config/eww/center_control"
--sidebar_control = spawn "exec ~/.config/eww/sidebar_control"
--maimcopy = spawn "maim -s | xclip -selection clipboard -t image/png && notify-send \"Screenshot\" \"Copied to Clipboard\" -i flameshot"
--maimsave = spawn "maim -s ~/Desktop/$(date +%Y-%m-%d_%H-%M-%S).png && notify-send \"Screenshot\" \"Saved to Desktop\" -i flameshot"
rofi_launcher = spawn "rofi -no-lazy-grab -show drun -modi run,drun,window -theme $HOME/.config/rofi/gruvbox-dark-hard -drun-icon-theme \"Papirus\" "

myKeys :: [(String, X ())]
myKeys =
    [
    -- Toggle screens
    ("M-<Rigth>", nextScreen)
    , ("M-<Left>", prevScreen)

    -- Toggle window no borders
    , ("M-g", withFocused toggleBorder)
    
    -- Multitoggle
    , ("M-S-<Tab>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
    , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)

    -- Launch a terminal
    , ("M-S-<Return>", spawn myTerminal)

    -- Launch rofi and dashboard
    , ("M-o", rofi_launcher)
    --, ("M-p", center_control)
    --("M-s", sidebar_control)

    -- Audio keys
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 toggle")

    -- Brightness keys
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl s +10%")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 10-%")
 
    -- Screenshot
    --("<Print>", maimcopy)
    --("M-<Print>", maimsave)

    -- Close focused window
    , ("M-S-c", kill)

    -- Gaps
    , ("M-C-g", sendMessage $ ToggleGaps) -- toggle all gaps
    , ("M-S-g", sendMessage $ setGaps [(L,30), (R,30), (U,30), (D,60)]) -- reset the GapSpec
    
    , ("M-C-t", sendMessage $ IncGap 10 L) -- increment the left-hand gap
    , ("M-S-t", sendMessage $ DecGap 10 L) -- decrement the left-hand gap
    
    , ("M-C-y", sendMessage $ IncGap 10 U) -- increment the top gap
    , ("M-C-y", sendMessage $ DecGap 10 U) -- decrement the top gap
    
    , ("M-C-u", sendMessage $ IncGap 10 D) -- increment the bottom gap
    , ("M-S-u", sendMessage $ DecGap 10 D) -- decrement the bottom gap

    , ("M-C-i", sendMessage $ IncGap 10 R) -- increment the right-hand gap
    , ("M-S-i", sendMessage $ DecGap 10 R) -- decrement the right-hand gap

     -- Rotate through the available layouts
    , ("M-<Space>", sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    --((modm .|. shiftMask, xK_space ), setLayout myLayout)

    -- Resize viewed windows to the correct size
    , ("M-n", refresh)

    -- Move focus to the next window
    , ("M-<Tab>", windows W.focusDown)

    -- Move focus to the next window
    , ("M-j", windows W.focusDown)

    -- Move focus to the previous window
    , ("M-k", windows W.focusUp)

    -- Move focus to the master window
    , ("M-m", windows W.focusMaster)

    -- Swap the focused window and the master window
    , ("M-<Return>", windows W.swapMaster)

    -- Swap the focused window with the next window
    , ("M-S-j", windows W.swapDown)

    -- Swap the focused window with the previous window
    , ("M-S-k", windows W.swapUp)

    -- Shrink the master area
    , ("M-h", sendMessage MirrorShrink)

    -- Expand the master area
    , ("M-l", sendMessage MirrorExpand)

    -- Toggle float window
    --, ("M-S-t", sendMessage (T.Toggle simplestFloat))

    -- Push window back into tiling
    , ("M-t", withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ("M-.", sendMessage (IncMasterN (-1)))

    -- Restart xmonad
    , ("M-q", spawn "xmonad --recompile; xmonad --restart")

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
--
myLayout = avoidStruts
    $ smartBorders
    $ mouseResize
    $ windowArrange
    $ draggingVisualizer
    $ mkToggle (NOBORDERS ?? NBFULL ?? EOT)
    $ tiled ||| Mirror tiled ||| Circle ||| noBorders Full
  where
    tiled   = ResizableTall nmaster delta ratio []
    nmaster = 1        -- The default number of windows in the master pane
    ratio   = 1/2      -- Default proportion of screen occupied by master pane
    delta   = 3/100    -- Percent of screen to increment by when resizing panes

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
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
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
    spawnOnce "polybar --config=~/.config/polybar/config.ini main"
    --spawnOnce "exec ~/bin/eww daemon"
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
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        -- Hooks
        manageHook         = insertPosition Below Newer <+> myManageHook, 
        layoutHook         = spacingRaw False (Border 6 6 6 6) True (Border 6 6 6 6) True $ myLayout,
        handleEventHook    = myEventHook,
        logHook            = myLogHook >> workspaceHistoryHook,
        startupHook        = myStartupHook >> addEWMHFullscreen
    } `additionalKeysP` myKeys
