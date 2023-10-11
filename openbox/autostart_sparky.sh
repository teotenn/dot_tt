#! /bin/bash

if [ -f /usr/bin/sparky-polkit ]; then
sparky-polkit &
fi
#if [ -f /usr/bin/nitrogen ]; then
#nitrogen --restore &
#fi
# if [ -f /usr/bin/tint2 ]; then
# tint2 &
# elif [ -f /usr/bin/fbpanel ]; then
# fbpanel &
# fi
if [ -f /usr/bin/xscreensaver ]; then
xscreensaver -nosplash &
fi
if [ -f /usr/bin/pnmixer ]; then
(sleep 5; pnmixer) &
fi
if [ -f /opt/sparky/nm-applet-reload ]; then
/opt/sparky/nm-applet-reload &
fi
if [ -f /usr/bin/xfce4-power-manager ]; then
/usr/bin/xfce4-power-manager &
fi
if [ -f /usr/bin/xdg-user-dirs-gtk-update ]; then
/usr/bin/xdg-user-dirs-gtk-update &
fi

# Personal autostart
/bin/bash /home/teoten/Code/dot_tt/bash/scripts/autostart.sh 
