#!/bin/bash

xcompmgr -c &
#compton -cGb
xsetroot -cursor_name left_ptr
#feh --bg-center .local/wallpapers/moon-earth_00384532.jpg
pcmanfm --desktop -d &
xrandr --output LVDS --set "scaling mode" "Full aspect"
emacs --daemon &

exec xmonad

