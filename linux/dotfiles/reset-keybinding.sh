#! /bin/sh

xrdb -merge ./.Xresources 
xmodmap ./.Xmodmap
setxkbmap -rules evdev -model jp106 -layout jp
setxkbmap -option "ctrl:swapcaps"
xmodmap ./.Xmodmap


