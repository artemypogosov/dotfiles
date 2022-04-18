#!/usr/bin/bash

# Cursor active at boot
xsetroot -cursor_name left_ptr &

# Starting applications at boot time

# picom &
udiskie &
emacs --daemon &
nitrogen --restore &
unclutter --timeout 10 &
dunst &
guake &
nm-applet &
volumeicon &
blueman-applet &
dropbox &

#sleep 2 &&

#trayer --edge bottom --align right --widthtype request --padding 5 --SetDockType true --SetPartialStrut false --expand true --monitor 1 --transparent true --alpha 255 --height 20 &
#powerkit &
#lxqt-powermanagement &
