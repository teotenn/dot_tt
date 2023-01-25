#!/bin/bash

# Files to backup
rsync -av ~/.bash_aliases ~/Code/dot_tt/bash
rsync -av ~/.stumpwm.d/init.lisp ~/Code/dot_tt/stumpwm
rsync -arv --include="*.sh" --exclude="*" ~/bin/ ~/Code/dot_tt/scripts
rsync -raP ~/.config/openbox/ ~/Code/dot_tt/openbox/

# git add and commit
now=$(date)
cd ~/Code/dot_tt/
git add .
git commit -am "Auto backup on Debian '$now'"
