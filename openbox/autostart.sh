#! /bin/bash

if [ -f /usr/bin/sparky-polkit ]; then
sparky-polkit &
fi
if [ -f /usr/bin/tint2 ]; then
tint2 &
elif [ -f /usr/bin/fbpanel ]; then
fbpanel &
fi
if [ -f /usr/bin/xscreensaver ]; then
xscreensaver -nosplash &
fi
if [ -f /usr/bin/pnmixer ]; then
(sleep 5; pnmixer) &
fi
if [ -f /usr/bin/pcmanfm ]; then
pcmanfm -d &
elif [ -f /usr/bin/thunar ]; then
thunar --daemon &
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

sleep 2
/bin/bash /home/teoten/Code/personal_config/scripts/autostart.sh 

# /home/teoten/.guix-profile/bin/conky &
# /home/teoten/.guix-profile/bin/conky -c /home/teoten/Code/personal_config/conky/blue-white.conkyrc & exit
~/.guix-profile/bin/conky -q -c ~/Code/personal_config/conky/blue-white.conkyrc & ~/.guix-profile/bin/conky -q -c ~/Code/personal_config/conky/clock.conkyrc & exit
