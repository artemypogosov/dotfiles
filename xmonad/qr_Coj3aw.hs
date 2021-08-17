-- BASE
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

-- UTILS
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad

    -- Data
import Data.Monoid

-- HOOKS
import XMonad.Hooks.ManageDocks
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

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor   = "#282c34"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#46d9ff"  -- Border color of focused windows


myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4"]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


myStartupHook :: X ()
myStartupHook = do
  spawnOnce "picom &" -- compositor (fork of compton)
  spawnOnce "nitrogen --restore &"
  spawnOnce "powerkit &"
  spawnOnce "/usr/bin/emacs --daemon &" -- emacs daemon for the emacsclient
  setDefaultCursor xC_left_ptr
  setWMName "LG3D"


myLayout = avoidStruts (tall ||| Mirror tall ||| Full)
                  where  tall = Tall 1 (3/100) (1/2)

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "calculator" spawnCalc findCalc manageCalc]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCalc  = "qalculate-gtk"
    findCalc   = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out the full
     -- name of my workspaces and the names would be very long if using clickable workspaces.
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
     , title =? "Oracle VM VirtualBox Manager"  --> doFloat
     , title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 1 )
     , className =? "google-chrome-stable"   --> doShift ( myWorkspaces !! 1 )
     , className =? "qutebrowser"     --> doShift ( myWorkspaces !! 1 )
     , className =? "mpv"             --> doShift ( myWorkspaces !! 7 )
     , className =? "Gimp"            --> doShift ( myWorkspaces !! 8 )
     , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 4 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     , isFullscreen -->  doFullFloat
     ] <+> namedScratchpadManageHook myScratchPads

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
        , ("M-f", spawn (myFileManager))
        , ("M-<Escape>", spawn "dm-tool lock")
        , ("M-b", spawn "brave")
        , ("M-e", spawn "emacsclient -ca emacs")
        , ("<Print>", spawn "flameshot gui")
        , ("<M-j>", spawn "jetbrains-toolbox")

         -- Scratchpads
         -- Toggle show/hide these programs.  They run on a hidden workspace.
         -- When you toggle them to show, it brings them to your current workspace.
         -- Toggle them to hide and it sends them back to hidden workspace (NSP).
        , ("M-s", namedScratchpadAction myScratchPads "terminal")
        -- , ("C-s m", namedScratchpadAction myScratchPads "mocp")
        -- , ("C-s c", namedScratchpadAction myScratchPads "calculator")


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
  terminal      = myTerminal
  , modMask     = myModMask
  , startupHook = myStartupHook
  , manageHook  = myManageHook
  , layoutHook  = myLayout
  , workspaces  = myWorkspaces
  , borderWidth = myBorderWidth
  , normalBorderColor = myNormColor
  , focusedBorderColor = myFocusColor
  , logHook = dynamicLogWithPP xmobarPP {
                  ppCurrent = xmobarColor  "#51AFEF" "" . wrap "[" "]"
                , ppTitle = xmobarColor "#b3afc2" "" . shorten 30
                , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""  -- Hidden workspaces
                , ppHiddenNoWindows = xmobarColor "#c792ea" ""  -- Hidden workspaces (no windows)
                , ppVisible = xmobarColor "#98be65" ""          -- Visible but not current workspace
                , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"            -- Urgent workspace
                , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"                    -- Separator character
                , ppExtras = [windowCount]
                , ppOutput = hPutStrLn xmproc
         }
    } `additionalKeysP` myKeys
