{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           Graphics.X11.ExtraTypes.XF86
import           System.IO (hPutStrLn)
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.GridVariants (Grid(..))
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys)
import           XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP)
import           XMonad.Util.Run (spawnPipe)
import           XMonad.Util.Scratchpad

myTerminal = "uxterm"

myLayoutHook = avoidStruts $ smartSpacing 1 $ smartBorders $ Grid (16/9) ||| Tall 1 (3/100) (2/3) ||| Full

myManageHook = composeAll
  [ manageDocks
  , isFullscreen --> doFullFloat
  , className =? "arx" --> doFullFloat
  , className =? "wesnoth" --> doFullFloat
  , className =? "Minetest" --> doFullFloat
  , className =? "scummvm" --> doFullFloat
  , className =? "Wxlauncher" --> doFloat
  , className =? "Galculator" --> doFloat
  , className =? "Skype" --> doFloat
  , scratchpadManageHook (W.RationalRect 0.25 0.25 0.5 0.5)
  ]

myStartupHook = do
  spawn "compton --config ~/.config/compton.conf -b"
  spawn "feh --bg-center ~/Documents/Wallpapers/nyc.jpg"
  setWMName "LG3D"

myKeys = [
  ((mod4Mask, xK_BackSpace), kill)
  , ((mod4Mask, xK_q), kill)
  , ((mod4Mask, xK_Return), spawn myTerminal)
  , ((mod4Mask, xK_m), windows W.swapMaster)
  , ((mod4Mask, xK_s), scratchpadSpawnAction myConfig)
  , ((mod4Mask, xK_a), spawn "xmenud")
  , ((mod4Mask, xK_e), spawn "emacs")
  , ((mod4Mask, xK_w), runOrRaise "firefox" (className =? "Firefox"))
  , ((mod4Mask, xK_f), spawn "pcmanfm")
  , ((mod4Mask, xK_Right), windowGo R False)
  , ((mod4Mask, xK_Left), windowGo L False)
  , ((mod4Mask, xK_Up), windowGo U False)
  , ((mod4Mask, xK_Down), windowGo D False)
  , ((mod4Mask .|. mod1Mask, xK_Right), windowSwap R False)
  , ((mod4Mask .|. mod1Mask, xK_Left), windowSwap L False)
  , ((mod4Mask .|. mod1Mask, xK_Up), windowSwap U False)
  , ((mod4Mask .|. mod1Mask, xK_Down), windowSwap D False)
  , ((mod4Mask, xK_comma), prevWS)
  , ((mod4Mask, xK_period), nextWS)
  , ((mod4Mask, xK_d), moveTo Next EmptyWS)
  , ((shiftMask, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 1")
  , ((shiftMask, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 1")
  , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10")
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
  , ((0, 0x1008FF12), spawn "~/.xmonad/volume.py toggle_mute")
  , ((0, 0x1008FF13), spawn "~/.xmonad/volume.py inc_volume")
  , ((0, 0x1008FF11), spawn "~/.xmonad/volume.py dec_volume")
  , ((mod4Mask, xK_p), spawn "dmenu_run -fn 'Noto Sans-9' -nb '#101010' -nf white -sb '#101010' -sf '#ee9a00'")
  , ((mod4Mask .|. controlMask, xK_r), spawn "xmonad --recompile && xmonad --restart")
  ]

myConfig = ewmh $ def {
  terminal = myTerminal
  , modMask = mod4Mask
  , borderWidth = 0
  , normalBorderColor = "#101010"
  , focusedBorderColor = "#101010"
  , layoutHook = myLayoutHook
  , manageHook = myManageHook
  , handleEventHook = fullscreenEventHook <+> docksEventHook
  , logHook = fadeInactiveLogHook 0.8
  , startupHook = myStartupHook
  } `additionalKeys` myKeys


main = do
  statusbar <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  
  let xmobarLogHook = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP
        { ppCurrent = xmobarColor "#d7af87" ""
        , ppHidden = xmobarColor "#666666" ""
        , ppHiddenNoWindows = \_ -> ""
        , ppUrgent = xmobarColor "#a36666" ""
        , ppOrder = \(ws:_:t:_) -> [ws, t]
        , ppOutput = hPutStrLn statusbar
        , ppWsSep = "  "
        , ppSep = "  |  "
        }
  
  xmonad myConfig { logHook = logHook myConfig >> xmobarLogHook }

