#!/bin/sh

echo 1901 | sudo -S echo 0 | sudo tee /sys/class/leds/platform\:\:micmute/brightness

exit 0
