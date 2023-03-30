if [ -f /usr/bin/thunar ]; then
thunar --daemon &
fi

if [ -f /usr/bin/emacs ]; then
emacs --daemon &
fi

sleep 2
/bin/bash /home/teoten/Code/dot_tt/bash/scripts/sirius_xrandr.sh

if [ -f /usr/bin/conky ]; then
    conky -q -c ~/Code/dot_tt/conky/blue-white.conkyrc & conky -q -c ~/Code/dot_tt/conky/clock.conkyrc &
elif [ -f ~/.guix-profile/bin/conky ]; then
    ~/.guix-profile/bin/conky -q -c ~/Code/dot_tt/conky/blue-white.conkyrc & ~/.guix-profile/bin/conky -q -c ~/Code/dot_tt/conky/clock.conkyrc &
fi
