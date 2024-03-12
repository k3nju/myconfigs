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
		 --rate 250 \
		 --d1 1500 \
		 --d2 2000 \
		 --d3 2500 \
		 --d4 4000 \
		 --default-dpi 2 \
		 --no-dpishift \
		 --print f3

# excel
#sudo ratslap -m f4 --middle modeswitch
sudo ratslap \
		 --modify f4 \
		 --color green \
		 --middle modeswitch \
		 --g4 esc \
		 --g5 rightctrl+c \
		 --g6 rightctrl+z \
		 --g7 rightctrl+v \
		 --g8 rightctrl+pageup \
		 --g9 rightctrl+pagedown \
		 --rate 500 \
		 --d1 1500 \
		 --d2 2000 \
		 --d3 2500 \
		 --d4 4000 \
		 --default-dpi 2 \
		 --no-dpishift \
		 --print f4

# ghidra
sudo ratslap \
		 --modify f5 \
		 --color red \
		 --middle modeswitch \
		 --g4 rightalt+right \
		 --g5 rightalt+left \
		 --g6 pagedown \
		 --g7 pageup \
		 --g8 rightctrl+rightshift+tab \
		 --g9 rightctrl+tab \
		 --rate 250 \
		 --d1 1500 \
		 --d2 2000 \
		 --d3 2500 \
		 --d4 4000 \
		 --default-dpi 2 \
		 --no-dpishift \
		 --print f5
