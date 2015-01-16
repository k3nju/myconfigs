#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

# keyboard
# "xmodmap -e" is test mode. so this configuration is not persistent
# use intead. xmodmap -pke > ./.Xmodmap and load it as below
#xmodmap -e "keysym Muhenkan = Alt_R Meta_R" && echo $? > /tmp/a
#xmodmap -e "keycode 66 = Control_L Control_L Control_L Control_L" # shift+caps as ctrl

PATH_TO_dotXmodmap=
/usr/bin/xmodmap ${PATH_TO_dotXmodmap}

# setxkbmap must be called after xmodmap
setxkbmap -rules evdev -model jp106 -layout jp
setxkbmap -option "ctrl:swapcaps"

# ---- ibus
#export XIM=ibus
#export XIM_PROGRAM=/usr/bin/ibus-daemon
#export XIM_ARGS="--xim"
#export XMODIFIERS="@im=ibus"
#export GTK_IM_MODULE="ibus"
#export QT_IM_MODULE="ibus"
#ibus-daemon -d -x

# ---- fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"

# start window manager
exec startxfce4
#exec ck-launch-session startxfce4
#exec ck-launch-session gnome-session
