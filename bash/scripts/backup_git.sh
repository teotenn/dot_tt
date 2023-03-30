#!/bin/bash

# Files to backup
rsync -av ~/.bashrc ~/Code/dot_tt/bash

# dot files for WMs
if [ -f ~/.stumpwm.d ]; then
    rsync -av ~/.stumpwm.d/init.lisp ~/Code/dot_tt/stumpwm
fi
if [ -f ~/.config/openbox/rc.xml ]; then
    rsync -raP ~/.config/openbox/ ~/Code/dot_tt/openbox/
fi
if [ -f ~/.config/rofi/launcher.sh ]; then
    rsync -raP ~/.config/rofi/ ~/Code/dot_tt/rofi/
fi

# git add and commit
# now=$(date)
# cd ~/Code/dot_tt/
# git add .
# git commit -am "Auto backup from Debian '$now'"
