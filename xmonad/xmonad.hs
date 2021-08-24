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

-- LAYOUT
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

-- LAYOUTS MODIFIERS
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Simplest
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.ToggleLayouts 
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- HOOKS
import XMonad.Hooks.ManageDocks(docks, avoidStruts, ToggleStruts(..))
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.DynamicLog(xmobarPP
                              , wrap
                              , dynamicLogWithPP
                              , xmobarColor
                              , ppCurrent
                              , ppVisible
                              , ppSep
                              , ppTitle
                              , ppUrgent
                              , ppHidden
                              , ppHiddenNoWindows
                              , ppExtras
                              , shorten
                              , ppOutput)
import Graphics.X11.ExtraTypes.XF86

-- VARS
myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myFileManager :: String
myFileManager = "pcmanfm"

myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "  -- Sets emacs as editor


myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor   = "#282c34"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#46d9ff"  -- Border color of focused windows


myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5"]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
  -- spawnOnce "picom &"                   -- compositor (fork of compton)
  spawnOnce "nitrogen --restore &"      -- set wallpaper
  spawnOnce "powerkit &"                -- power manager
  spawnOnce "trayer --edge bottom --align right --widthtype request --padding 5 --SetDockType true --SetPartialStrut false --expand true --monitor 1 --transparent true --alpha 256 --height 20 &"
  spawnOnce "nm-applet &"
  spawnOnce "blueman-applet &"
  spawnOnce "pasystray &"
  spawnOnce "/home/artemy/Scripts/init-us.sh"
  spawnOnce "/home/artemy/Scripts/fix-mic-led.sh"
  spawnOnce "/usr/bin/emacs --daemon &" -- emacs daemon for the emacsclient
  setDefaultCursor xC_left_ptr
  setWMName "LG3D"

-- if fewer than two windows. So a single window has no gaps.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 4
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 20 simplestFloat
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabTheme
threeCol = renamed [Replace "threeCol"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)


-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

myLayoutHook = avoidStruts $ toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder myBorderWidth tall
                                 ||| noBorders monocle
                                 ||| floats
                                 ||| noBorders tabs

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm ]

  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     [ className =? "confirm"         --> doFloat
     , className =? "file_progress"   --> doFloat
     , className =? "dialog"          --> doFloat
     , className =? "download"        --> doFloat
     , className =? "error"           --> doFloat
     , className =? "Gimp"            --> doFloat
    -- , className =? "Guake"           --> doFloat
     , className =? "notification"    --> doFloat
     , className =? "jetbrains-idea"  --> doFloat
     , className =? "pinentry-gtk-2"  --> doFloat
     , className =? "splash"          --> doFloat
     , className =? "toolbar"         --> doFloat
     , title     =? "Oracle VM VirtualBox Manager"  --> doFloat
     , className =? "google-chrome-stable"   --> doShift ( myWorkspaces !! 1)
     , className =? "qutebrowser"     --> doShift ( myWorkspaces !! 1 )
     , className =? "mpv"             --> doShift ( myWorkspaces !! 7 )
     , className =? "Gimp"            --> doShift ( myWorkspaces !! 8 )
     , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 4 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     , isFullscreen -->  doFullFloat
     ] <+> namedScratchpadManageHook myScratchPads

unusedKeys :: [(String)]
unusedKeys = [ ("M-<Space>") ]

myKeys :: [(String, X ())]
myKeys =
        -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")  -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")    -- Restarts xmonad
        , ("M-S-q", io exitSuccess)              -- Quits xmonad

        -- Run Prompt
        , ("M-S-<Return>", spawn "dmenu_run -i -p \"Run: \"") -- Dmenu

        -- Useful programs to launch
        , ("M-<Return>", spawn (myTerminal))
        , ("M-f",        spawn (myFileManager))
        , ("M-x", spawn ("Scripts/xmenu/xmenu.sh"))
        -- , ("M-S-e",      spawn "emacsclient -ca emacs")
        , ("M-S-e",        spawn "emacsclient -ca emacs")

        -- TODO: add more keybindings to flameshot
        , ("<Print>",    spawn "flameshot gui")

         -- Scratchpads
         -- Toggle show/hide these programs.  They run on a hidden workspace.
         -- When you toggle them to show, it brings them to your current workspace.
         -- Toggle them to hide and it sends them back to hidden workspace (NSP).
        , ("M-s", namedScratchpadAction myScratchPads "terminal")

        -- Other
        , ("M-m", sendMessage ToggleStruts)
        , ("M-<Space>", spawn "/home/artemy/Scripts/layout-switcher.sh")
        , ("M-<Delete>", spawn "dm-tool switch-to-greeter && systemctl suspend")
        , ("M-<Escape>", spawn "dm-tool switch-to-greeter")
        -- Kill windows
        , ("M-S c", kill1)     -- Kill the currently focused client
        , ("M-S-a", killAll)   -- Kill all windows on current workspace

        -- Layout
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout

        -- Laptop specific
        , ("<XF86AudioMute>", spawn "amixer set Master toggle")
        , ("<XF86AudioMicMute>", spawn "amixer set Capture toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%-")
        , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+")


         , ("<XF86AudioPlay>", spawn "playerctl play-pause")
         , ("<XF86AudioPrev>", spawn "playerctl previous")
         , ("<XF86AudioNext>", spawn "playerctl next")

         , ("<XF86MonBrightnessUp>", spawn "lux -a 5%")
         , ("<XF86MonBrightnessDown>", spawn "lux -s 5%")
        ]

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar $HOME/.config/xmobar/xmobarrc"
  xmonad $ docks def  {
  terminal             = myTerminal
  , modMask            = myModMask
  , startupHook        = myStartupHook
  , manageHook         = myManageHook
  , layoutHook         = myLayoutHook
  , workspaces         = myWorkspaces
  , borderWidth        = myBorderWidth
  , normalBorderColor  = myNormColor
  , focusedBorderColor = myFocusColor
  , logHook = dynamicLogWithPP xmobarPP {
                  ppCurrent         = xmobarColor  "#51AFEF" "" . wrap "[" "]"
                , ppTitle           = xmobarColor "#b3afc2" "" . shorten 30
                , ppHidden          = xmobarColor "#82AAFF" "" . wrap "*" ""    -- Hidden workspaces
                , ppHiddenNoWindows = xmobarColor "#c792ea" ""                  -- Hidden workspaces (no windows)
                , ppVisible         = xmobarColor "#98be65" ""                  -- Visible but not current workspace
                , ppUrgent          = xmobarColor "#C45500" "" . wrap "!" "!"   -- Urgent workspace
                , ppSep             =  "<fc=#888> <fn=1>|</fn> </fc>"           -- Separator character
                , ppExtras          = [windowCount]
                , ppOutput          = hPutStrLn xmproc
         }
    } `removeKeysP` unusedKeys
      `additionalKeysP` myKeys
