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

# ghidra
sudo ratslap -m f4 --middle modeswitch
#sudo ratslap \
#		 --modify f4 \
#		 --color red \
#		 --g4 leftalt+n \
#		 --g5 leftalt+p \
#		 --g6 leftalt+f \
#		 --g7 leftalt+c \
#		 --print f4

# excel
sudo ratslap -m f5 --middle modeswitch
#sudo ratslap -m f5 -c green -p f5


