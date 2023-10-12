#!/bin/bash

if [ -f /usr/bin/thunar ] || [ -f ~/.guix-profile/bin/thunar ]; then
    thunar --daemon &
fi

if [ -f /usr/bin/compton ] || [ -f ~/.guix-profile/bin/compton ]; then
    compton &
fi

if [ -f /usr/bin/emacs ] || [ -f ~/.guix-profile/bin/emacs ]; then
    emacs --daemon &
fi

sleep 2
if [ -f /home/teoten/Code/dot_tt/bash/scripts/sirius_xrandr.sh ]; then
    bash /home/teoten/Code/dot_tt/bash/scripts/sirius_xrandr.sh
fi

if [ -f /usr/bin/conky ] || [ -f ~/.guix-profile/bin/conky ]; then
    conky -q -c ~/Code/dot_tt/conky/blue-white.conkyrc &
fi

if [ -f /usr/bin/tint2 ] || [ -f ~/.guix-profile/bin/tint2 ]; then
    tint2 -c /home/teoten/Code/dot_tt/tint2/minima/minima.tint2rc &
fi
