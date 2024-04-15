#!/usr/bin/bash

# Cursor active at boot
xsetroot -cursor_name left_ptr &

# Starting applications at boot time

picom --legacy-backends &
dunst &
udiskie &
lxqt-powermanagement &
emacs --daemon &
nitrogen --restore &
unclutter --timeout 10 &
guake &
nm-applet &
#volumeicon &
blueman-applet &
dropbox &
setxkbmap -option "ctrl:nocaps" &
xset r rate 400 50 &

#sleep 2 &&

#trayer --edge bottom --align right --widthtype request --padding 5 --SetDockType true --SetPartialStrut false --expand true --monitor 1 --transparent true --alpha 255 --height 20 &
