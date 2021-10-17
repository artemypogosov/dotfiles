-- BASE
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- UTILS
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad

-- ACTIONS
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (killAll)

-- DATA
import Data.Monoid
import qualified Data.Map.Strict as M
import Data.Ratio -- this makes the '%' operator available

-- LAYOUT
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

-- LAYOUTS MODIFIERS
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import XMonad.Hooks.RefocusLast (refocusLastLayoutHook, refocusLastWhen, isFloat)
import XMonad.Layout.LayoutHints
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Grid
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.SubLayouts
import XMonad.Layout.ToggleLayouts
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- HOOKS
import XMonad.Hooks.ManageDocks(docks, avoidStruts, ToggleStruts(..))
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat, isDialog)
import XMonad.Hooks.DynamicLog
import Graphics.X11.ExtraTypes.XF86

-- OTHER
import Control.Monad (liftM2)

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myFileManager :: String
myFileManager = "pcmanfm"

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myBorderWidth :: Dimension
myBorderWidth = 1

myNormColor :: String
myNormColor   = "#282c34"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#46d9ff"  -- Border color of focused windows

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Change workspaces when I got two 27 inch monitors
myWorkspaces :: [String]
myWorkspaces = ["web", "dev", "3", "4", "5"]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
  -- spawnOnce "picom &" -- compositor (fork of compton)
  spawnOnce "$HOME/.xmonad/scripts/autostart.sh"
  spawnOnce "$HOME/Scripts/init-us.sh"
  spawnOnce "$HOME/Scripts/fix-mic-led.sh"
  setDefaultCursor xC_left_ptr
  setWMName "LG3D"


-- window manipulations
myManageHook = composeAll . concat $
    [ [isDialog       --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title     =? t --> doFloat       | t <- myTFloats]
    , [resource  =? r --> doFloat       | r <- myRFloats]
    , [resource  =? i --> doIgnore      | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "web" | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "dev" | x <- my2Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61947" | x <- my3Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61635" | x <- my4Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61502" | x <- my5Shifts]
    ]
  where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["confirm", "file_progress", "download", "error", "notification"
               , "toolbar", "Oracle VM VirtualBox Manager", "jetbrains-idea" , "guake-toggle"
               , "Guake", "Zathura", "Arandr", "Galculator"]
    myTFloats = ["Downloads", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window"]
    my1Shifts = ["Google-chrome", "Firefox"]
    my2Shifts = ["Emacs"]
    -- my3Shifts = ["Inkscape"]
    -- my4Shifts = []
    -- my5Shifts = ["Gimp", "feh"]


-- If fewer than two windows. So a single window has no gaps.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = "#928374"
                 , activeBorderColor   = "#928374"
                 , inactiveColor       = "#32302f"
                 , inactiveBorderColor = "#32302f"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

myTall = renamed [Replace "tall"]
  $ smartBorders
  $ windowNavigation
  $ subLayout [] (smartBorders Simplest)
  $ limitWindows 12
  $ mySpacing 5
  $ ResizableTall 1 (3/100) (1/2) []

mySpiral = renamed [Replace "spiral"]
  $ smartBorders
  $ mySpacing 5
  $ limitWindows 12
  $ spiral (125 % 146)

myGrid = renamed [Replace "grid"]
  $ smartBorders
  $ mySpacing 5
  $ limitWindows 12
  $ Grid

myFloat = renamed [Replace "float"]
  $ smartBorders
  $ mySpacing 5
  $ limitWindows 12
  $ simplestFloat

myMirror = renamed [Replace "mirror tall"]
  $ smartBorders
  $ mySpacing 5
  $ limitWindows 12
  $ Mirror myTall

myTabs = renamed [Replace "tabs"]
  $ tabbed shrinkText myTabTheme

myFull = Full

myLayoutHook = withBorder myBorderWidth myLayouts
  where
    myLayouts = myTall ||| mySpiral ||| myGrid ||| myMirror ||| myFull ||| myFloat ||| myTabs

myKeys :: [(String, X ())]
myKeys =
        --- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")  -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")    -- Restarts xmonad
        , ("M-S-q", io exitSuccess)              -- Quits xmonad

        --- Run dmenu
        , ("M-S-<Return>", spawn "dmenu_run -i -p \"Run: \"")

        --- Useful programs to launch
        , ("M-<Return>", spawn myTerminal)
        , ("M-f",        spawn myFileManager)
        -- , ("M-x", spawn "Scripts/xmenu/xmenu.sh")
        , ("M-<Delete>", spawn "xkill")
        , ("M-e",        spawn "emacsclient -ca emacs")

        -- Flameshot
        , ("C-S-<Print>", spawn "flameshot gui     -p ~/Pictures/Screenshots/SS")
        , ("S-<Print>",   spawn "flameshot full -c -p ~/Pictures/Screenshots/SS")
        , ("<Print>",     spawn "flameshot full    -p ~/Pictures/Screenshots/SS")
        -- , ("M-s", namedScratchpadAction myScratchPads "terminal")

        -- Other
        --, ("M-m",        sendMessage ToggleStruts)
        , ("M-m", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-<Space>",  spawn "/home/artemy/Scripts/layout-switcher.sh")
        , ("M-<End>",    spawn "systemctl suspend")
        , ("M-<Escape>", spawn "i3lock -c 000000")
        , ("S-<Space>",  spawn "setxkbmap ua")

        -- Kill windows
        , ("M-S-c", kill1)
        , ("M-S-a", killAll)

        -- Layout
        , ("M-<Tab>", sendMessage NextLayout)

        -- Laptop specific
        , ("<XF86AudioMute>",        spawn "amixer set Master toggle")
        , ("<XF86AudioMicMute>",     spawn "amixer set Capture toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%-")
        , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+")


        , ("<XF86AudioPlay>", spawn "playerctl play-pause")
        , ("<XF86AudioPrev>", spawn "playerctl previous")
        , ("<XF86AudioNext>", spawn "playerctl next")

        , ("<XF86MonBrightnessUp>",   spawn "lux -a 5%")
        , ("<XF86MonBrightnessDown>", spawn "lux -s 5%")
        ]


myEventHook = refocusLastEventHook <+> hintsEventHook <+> fullscreenEventHook
    where
        refocusLastEventHook = refocusLastWhen isFloat

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc-laptop"
  xmonad $ docks def  {
  terminal             = myTerminal
  , modMask            = myModMask
  , startupHook        = myStartupHook
  , manageHook         = myManageHook
  , layoutHook         = avoidStruts $ toggleLayouts myFloat $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ smartBorders myLayoutHook
  , workspaces         = myWorkspaces
  , borderWidth        = myBorderWidth
  , normalBorderColor  = myNormColor
  , handleEventHook    = myEventHook
  , focusedBorderColor = myFocusColor
  , focusFollowsMouse  = myFocusFollowsMouse
  , logHook =  dynamicLogWithPP xmobarPP {
                  ppCurrent         = xmobarColor "#458588" "" . wrap "[" "]"
                , ppTitle           = xmobarColor "#b3afc2" "" . shorten 30
                , ppHidden          = xmobarColor "#83a598" "" . wrap "*" ""    -- Hidden workspaces
                , ppHiddenNoWindows = xmobarColor "#928374" ""                  -- Hidden workspaces (no windows)
                , ppVisible         = xmobarColor "#98971a" ""                  -- Visible but not current workspace
                , ppUrgent          = xmobarColor "#C45500" "" . wrap "!" "!"   -- Urgent workspace
                , ppSep             =  "<fc=#888> <fn=1>|</fn> </fc>"           -- Separator character
                , ppExtras          = [windowCount]
                , ppOrder           = \(ws:l:t:wc) -> [ws, l, head wc, t]
                , ppOutput          = hPutStrLn xmproc
         }
    } `additionalKeysP` myKeys
