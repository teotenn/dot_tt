#!/bin/bash

# Files to backup
rsync -av ~/.bash_aliases ~/Code/dot_tt/bash
rsync -arv --include="*.sh" --exclude="*" ~/bin/ ~/Code/dot_tt/bash/scripts

# dot files for WMs
rsync -av ~/.stumpwm.d/init.lisp ~/Code/dot_tt/stumpwm
rsync -raP ~/.config/openbox/ ~/Code/dot_tt/openbox/

# git add and commit
now=$(date)
cd ~/Code/dot_tt/
git add .
git commit -am "Auto backup from Debian '$now'"
