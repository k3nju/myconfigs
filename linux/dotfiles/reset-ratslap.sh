#! /bin/sh


sudo ratslap -m f3 \
-c black \
-6 pagedown \
-7 pageup \
-8 rightctrl+rightshift+tab \
-9 rightctrl+tab \
-U \
--d2 2000 \
-F 2 \
-r 250 \
-p f3

sudo ratslap -m f4 -c red -4 leftalt+n -5 leftalt+p -6 leftalt+f -7 leftalt+c -9 -p f4
sudo ratslap -m f5 -c green -p f5


