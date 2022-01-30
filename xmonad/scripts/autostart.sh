#!/usr/bin/bash

# Why do we need this?
function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

# Cursor active at boot
xsetroot -cursor_name left_ptr &

# Starting applications at boot time
trayer --edge bottom --align right --widthtype request --padding 5 --SetDockType true --SetPartialStrut false --expand true --monitor 1 --transparent true --alpha 256 --height 20 &
picom &
udiskie &
emacs --daemon &
nitrogen --restore &
unclutter --timeout 10 &

sleep 5s

guake $
nm-applet &
dropbox $
blueman-applet &
dunst &
#powerkit &
#lxqt-powermanagement &
