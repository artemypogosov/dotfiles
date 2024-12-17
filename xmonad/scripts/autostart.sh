#!/usr/bin/bash

# Cursor active at boot
xsetroot -cursor_name left_ptr &

# Remap ecsape to capslock button
#setxkbmap -option caps:escape &

# Starting applications at boot time

picom &
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
#setxkbmap -option "ctrl:nocaps" &
setxkbmap -option caps:escape &
xset r rate 400 50 &

#sleep 2 &&

#trayer --edge bottom --align right --widthtype request --padding 5 --SetDockType true --SetPartialStrut false --expand true --monitor 1 --transparent true --alpha 255 --height 20 &
