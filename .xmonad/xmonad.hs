import XMonad
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Actions.CycleWS
import XMonad.Util.EZConfig (additionalKeys)
import Graphics.X11.ExtraTypes.XF86

myConfig = ewmh $ defaultConfig {
  terminal = "xterm"
  , modMask = mod4Mask
  , borderWidth = 1
  , normalBorderColor = "#101010"
  , focusedBorderColor = "#101010"
  , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
  , manageHook = manageDocks <+> (isFullscreen --> doFullFloat) <+> manageHook defaultConfig
  , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook <+> docksEventHook
  , logHook = fadeInactiveLogHook 0.8
  } `additionalKeys` myKeys

myKeys = [
  ((mod4Mask, xK_BackSpace), kill)
  , ((mod4Mask, xK_c), kill)
  , ((mod4Mask, xK_Return), spawn $ terminal myConfig)
  , ((mod4Mask, xK_e), spawn "emc")
  , ((mod4Mask, xK_w), spawn "chromium")
  , ((mod4Mask, xK_f), spawn "pcmanfm")
  , ((mod4Mask, xK_Left), prevWS)
  , ((mod4Mask, xK_Right), nextWS)
  , ((mod4Mask, xK_d), moveTo Next EmptyWS)
  , ((0, 0x1008FF12), spawn "/home/robin/.xmonad/volume.py toggle_mute")
  , ((0, 0x1008FF13), spawn "/home/robin/.xmonad/volume.py inc_volume")
  , ((0, 0x1008FF11), spawn "/home/robin/.xmonad/volume.py dec_volume")
  , ((mod4Mask, xK_p), spawn "dmenu_run -fn 'Noto Sans-9' -nb '#101010' -nf white -sb '#101010' -sf '#ee9a00'")
  ]
  

main = xmonad =<< xmobar myConfig

