#!/bin/bash

xcompmgr -c &
#compton -cGb
feh --bg-center .local/wallpapers/moon-earth_00384532.jpg
xrandr --output LVDS --set "scaling mode" "Full aspect"
emacs --daemon &

exec xmonad

