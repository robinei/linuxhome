import           Graphics.X11.ExtraTypes.XF86
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.GridVariants (Grid(..))
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig (additionalKeys)


myConfig = ewmh $ def {
  terminal = "xterm"
  , modMask = mod4Mask
  , borderWidth = 1
  , normalBorderColor = "#101010"
  , focusedBorderColor = "#101010"
  , layoutHook = avoidStruts $ smartBorders $ Grid (16/9) ||| Tall 1 (3/100) (2/3) ||| Full
  , manageHook = composeAll
    [ manageDocks
    , isFullscreen --> doFullFloat
    , className =? "arx" --> doFullFloat
    , className =? "wesnoth" --> doFullFloat
    , className =? "Minetest" --> doFullFloat
    , className =? "Galculator" --> doFloat
    , className =? "Skype" --> doFloat
    ]
  , handleEventHook = fullscreenEventHook <+> docksEventHook
  , logHook = fadeInactiveLogHook 0.8
  } `additionalKeys` myKeys


myKeys = [
  ((mod4Mask, xK_BackSpace), kill)
  , ((mod4Mask, xK_c), kill)
  , ((mod4Mask, xK_Return), spawn $ terminal myConfig)
  , ((mod4Mask, xK_m), windows W.swapMaster)
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
  , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 1")
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 1")
  , ((0, 0x1008FF12), spawn "/home/robin/.xmonad/volume.py toggle_mute")
  , ((0, 0x1008FF13), spawn "/home/robin/.xmonad/volume.py inc_volume")
  , ((0, 0x1008FF11), spawn "/home/robin/.xmonad/volume.py dec_volume")
  , ((mod4Mask, xK_p), spawn "dmenu_run -fn 'Noto Sans-9' -nb '#101010' -nf white -sb '#101010' -sf '#ee9a00'")
  ]


main :: IO ()
main = xmonad =<< xmobar myConfig

