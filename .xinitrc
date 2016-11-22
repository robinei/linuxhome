#!/bin/bash

compton --config ~/.config/compton.conf -b
xsetroot -cursor_name left_ptr
feh --bg-center ~/Documents/Wallpapers/kyoto.jpg
#pcmanfm --desktop -d &
xrandr --output eDP1 --set "scaling mode" "Full aspect"
setxkbmap -option ctrl:nocaps

exec xmonad

