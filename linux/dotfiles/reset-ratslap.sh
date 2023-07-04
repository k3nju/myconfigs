#! /bin/sh

# default usage
sudo ratslap \
		 --modify f3 \
		 --color black \
		 --middle modeswitch \
		 --g4 rightalt+right \
		 --g5 rightalt+left \
		 --g6 pagedown \
		 --g7 pageup \
		 --g8 rightctrl+rightshift+tab \
		 --g9 rightctrl+tab \
		 --d2 2000\
		 --no-dpishift \
		 --default-dpi 2 \
		 --rate 250 \
		 --print f3

# excel
#sudo ratslap -m f4 --middle modeswitch
sudo ratslap \
		 --modify f4 \
		 --color green \
		 --middle modeswitch \
		 --g4 left \
		 --g5 right \
		 --g6 pagedown \
		 --g7 pageup \
		 --g8 leftctrl+pageup \
		 --g9 leftctrl+pagedown \
		 --d2 2000\
		 --no-dpishift \
		 --default-dpi 2 \
		 --rate 250 \
		 --print f4

# ghidra
sudo ratslap -m f5 --middle modeswitch
#sudo ratslap -m f5 -c green -p f5


