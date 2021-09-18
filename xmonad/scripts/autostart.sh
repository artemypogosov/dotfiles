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
nitrogen --restore &
powerkit &
trayer --edge bottom --align right --widthtype request --padding 5 --SetDockType true --SetPartialStrut false --expand true --monitor 1 --transparent true --alpha 256 --height 20 &
nm-applet &
blueman-applet &
unclutter --timeout 5 &
/usr/bin/emacs --daemon &
