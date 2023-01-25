#!/bin/bash

# Files to backup
rsync -av ~/.bash_aliases ~/Code/personal_config
rsync -av ~/.stumpwm.d/init.lisp ~/Code/personal_config/stumpwm
rsync -arv --include="*.sh" --exclude="*" ~/bin/ ~/Code/personal_config/scripts
rsync -raP ~/.config/openbox/ ~/Code/personal_config/openbox/

# git add and commit
now=$(date)
cd ~/Code/personal_config/
git add .
git commit -am "Backup on '$now'"
