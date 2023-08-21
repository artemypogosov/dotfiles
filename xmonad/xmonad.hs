-- BASE
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- UTILS
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (removeKeysP, mkNamedKeymap)
import XMonad.Util.Cursor
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad

-- ACTIONS
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (killAll)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)

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
import XMonad.Layout.ToggleLayouts as T
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- FIX FLOAT BEHAVIOR
import XMonad.Hooks.RefocusLast (refocusLastLayoutHook, refocusLastWhen, isFloat)
import XMonad.Layout.TrackFloating

-- HOOKS
import XMonad.Hooks.ManageDocks(docks, avoidStruts, ToggleStruts(..))
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat, isDialog)
import XMonad.Hooks.DynamicLog
import Graphics.X11.ExtraTypes.XF86

-- OTHER
import Control.Monad (liftM2)

-- VARS
myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "qutebrowser"

myFileManager :: String
myFileManager = "pcmanfm"

myFont :: String
myFont = "xft:Ubuntu:regular:size=9:antialias=true:hinting=true"

myBorderWidth :: Dimension
myBorderWidth = 2

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "

-- border color of normal windows
myNormColor :: String
myNormColor   = "#000000"  -- Border color of normal windows

-- border color of focused windows
myFocusColor :: String
myFocusColor  = "#cc241d"  -- Border color of focused windows

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Change workspaces when I got two 27 inch monitors
myWorkspaces :: [String]
myWorkspaces = ["web", "dev", "chat", "music", "other"]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
  spawn "killall trayer"
  spawnOnce "$HOME/.xmonad/scripts/autostart.sh"
  spawn "sleep 2 && trayer --edge bottom --align right --widthtype request --padding 5 --SetDockType true --SetPartialStrut false --expand true --monitor 1 --transparent true --alpha 256 --height 20"
  spawnOnce "$HOME/Scripts/init-us.sh"
  -- spawnOnce "$HOME/Scripts/fix-mic-led.sh"
  setDefaultCursor xC_left_ptr
  setWMName "LG3D"


-- window manipulations
myManageHook = composeAll . concat $
    [ [isDialog       --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title     =? t --> doFloat       | t <- myTFloats]
    , [resource  =? r --> doFloat       | r <- myRFloats]
    , [resource  =? i --> doIgnore      | i <- myIgnores]
   -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "web" | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "dev" | x <- my2Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61947" | x <- my3Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61635" | x <- my4Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61502" | x <- my5Shifts]
    ]
  where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["confirm", "file_progress", "download", "error", "notification"
               , "toolbar", "Oracle VM VirtualBox Manager", "jetbrains-idea"
               , "Arandr", "Galculator", "guake-toggle", "Guake"]
    myTFloats = ["Downloads", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window"]
    -- my1Shifts = ["Google-chrome", "qutebrowser"]
    my2Shifts = ["Emacs", "idea"]
    -- my3Shifts = ["Inkscape"]
    -- my4Shifts = []
    -- my5Shifts = ["Gimp", "feh"]


-- If fewer than two windows. So a single window has no gaps.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

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
  $ windowNavigation
  $ subLayout [] (smartBorders Simplest)
  $ limitWindows 5
  $ mySpacing 5
  $ ResizableTall 1 (3/100) (1/2) []

myGrid = renamed [Replace "grid"]
  $ mySpacing 5
  $ limitWindows 12
  $ Grid

myFloat = renamed [Replace "float"]
  $ mySpacing 5
  $ limitWindows 12
  $ simplestFloat

myMirror = renamed [Replace "mirror tall"]
  $ limitWindows 12
  $ Mirror myTall

myTabs = renamed [Replace "tabs"]
  $ noBorders
  $ tabbed shrinkText myTabTheme

myFull = renamed [Replace "full"]
  $ Full

myLayoutHook = refocusLastLayoutHook . trackFloating $ avoidStruts $ toggleLayouts myFloat $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ lessBorders Screen myLayouts
  where
    myLayouts = myTall ||| myMirror ||| myGrid ||| myFull ||| myFloat ||| myTabs

myKeys c = mkNamedKeymap c $
  -- General
  [ ("M-C-r",                  addName "Recompile XMonad"       $ spawn "xmonad --recompile")
  , ("M-S-r",                  addName "Restart XMonad"         $ spawn "xmonad --restart")
  , ("M-S-q",                  addName "Quit XMonad"            $ io exitSuccess)
  , ("M-S-c",                  addName "Kill focused window"    $ kill1)
  , ("M-S-a c",                addName "Kill all windows on WS" $ killAll)]

  ^++^ -- Layout
  [ ("M-S-m",                  addName "Swap focused W with master W"         $ windows W.swapMaster)
  , ("M-m",                    addName "Toggle full screen mode [no borders]" $ sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
  , ("M-<Tab>",                addName "Change layout"                        $ sendMessage NextLayout)]

  ^++^ -- Favorite programs
  [ ("M-S-<Return>",           addName "Launch Rofi"          $ spawn "rofi -show drun")
  , ("M-<Return>",             addName "Launch myTerminal"    $ spawn myTerminal)
 -- , ("M-b b",                  addName "Launch myBrowser"     $ spawn (myBrowser))
  , ("M-f f",                  addName "Launch myFileManager" $ spawn (myFileManager))
 -- , ("M-h h",                  addName "Launch htop"          $ spawn (myTerminal ++ " -e htop"))
 -- , ("M-r r",                  addName "Launch ranger"        $ spawn (myTerminal ++ " -e ranger"))
  ]

  ^++^ -- Custom
  [ ("M-<Space>",              addName "Switch keyboard layout" $ spawn "/home/artemy/Scripts/layout-switcher.sh")
  , ("M-<End>",                addName "Zzz..."                 $ spawn "systemctl suspend")
  , ("M-<Escape>",             addName "Lock screen"            $ spawn "betterlockscreen --lock dimblur")
  , ("M-r",                    addName "ru"                     $ spawn "setxkbmap ru")]

  ---h^++^ -- Floating windows
  --[ ("M-f",                    addName "Toggle float layout"      $ sendMessage (T.Toggle "float"))
  --, ("M-t",                    addName "Sink a floating window"   $ withFocused $ windows . W.sink)
  --, ("M-S-t",                  addName "Sink all floated windows" $ sinkAll)]

  ^++^ -- Doom Emacs
  [ ("M-d d",                  addName "Emacsclient"         $ spawn (myEmacs))
  , ("M-d b",                  addName "Emacsclient Ibuffer" $ spawn (myEmacs ++ ("--eval '(ibuffer)'")))
  , ("M-d f",                  addName "Emacsclient Dired"   $ spawn (myEmacs ++ ("--eval '(dired nil)'")))
  , ("M-d s",                  addName "Emacsclient Eshell"  $ spawn (myEmacs ++ ("--eval '(eshell)'")))
  , ("M-d v",                  addName "Emacsclient Vterm"   $ spawn (myEmacs ++ ("--eval '(+vterm/here nil)'")))]

  ^++^ -- Screenshot
  [ ("C-S-<Print>",            addName "Flameshot GUI"       $ spawn "flameshot gui")]

  ^++^ -- Multimedia
  [ ("<XF86AudioMute>",        addName "Mute audio"   $ spawn "amixer set Master toggle")
  , ("<XF86AudioMicMute>",     addName "Mute mic"     $ spawn "$HOME/Scripts/toggle-mic.sh")
  , ("<XF86AudioLowerVolume>", addName "Lower volume" $ spawn "amixer -q sset Master 5%-")
  , ("<XF86AudioRaiseVolume>", addName "Raise volume" $ spawn "amixer -q sset Master 5%+")

  , ("<XF86AudioPlay>",        addName "Play/Pause audio" $ spawn "playerctl play-pause")
  , ("<XF86AudioPrev>",        addName "Prev track"       $ spawn "playerctl previous")
  , ("<XF86AudioNext>",        addName "Nex track"        $ spawn "playerctl next")

  , ("<XF86MonBrightnessUp>",   addName "Brightness level up"   $ spawn "brightnessctl set +5%")
  , ("<XF86MonBrightnessDown>", addName "Brightness level down" $ spawn "brightnessctl set 5%-")]
  where nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

myEventHook = refocusLastEventHook <+> hintsEventHook
    where
        refocusLastEventHook = refocusLastWhen isFloat

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc"
  xmonad $ ewmhFullscreen $ addDescrKeys ((mod4Mask, xK_F1), xMessage) myKeys $ docks  def  {
    terminal              = myTerminal
  , modMask               = myModMask
  , startupHook           = myStartupHook
  , manageHook            = myManageHook
  , layoutHook            = myLayoutHook
  , workspaces            = myWorkspaces
  , borderWidth           = myBorderWidth
  , normalBorderColor     = myNormColor
  , handleEventHook       = myEventHook
  , focusedBorderColor    = myFocusColor
  , clickJustFocuses      = myClickJustFocuses
  , focusFollowsMouse     = myFocusFollowsMouse
  , logHook               = dynamicLogWithPP xmobarPP {
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
}
