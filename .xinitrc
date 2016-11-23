#!/bin/bash

#compton --config ~/.config/compton.conf -b
#feh --bg-center ~/Documents/Wallpapers/nyc.jpg
#pcmanfm --desktop -d &

xrandr --output eDP1 --set "scaling mode" "Full aspect"
xsetroot -cursor_name left_ptr
setxkbmap -option ctrl:nocaps

exec xmonad

