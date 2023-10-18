#!/bin/bash

## Compton
if [ -f /usr/bin/compton ] || [ -f ~/.guix-profile/bin/compton ]; then
    compton &
fi

## xrandr
sleep 2
if [ -f /home/teoten/Code/dot_tt/bash/scripts/sirius_xrandr.sh ]; then
    bash /home/teoten/Code/dot_tt/bash/scripts/sirius_xrandr.sh
fi

## conky
if [ -f /usr/bin/conky ] || [ -f ~/.guix-profile/bin/conky ]; then
    if [ "$DESKTOP_SESSION" = 'openbox' ]; then
	conky -q -c ~/Code/dot_tt/conky/blue-white.conkyrc &
    elif [ "$DESKTOP_SESSION" = 'qtile' ]; then
	conky -q -c ~/Code/dot_tt/conky/blue-white-qtile.conkyrc &
    fi
fi

## Tint2
if [ "$DESKTOP_SESSION" = 'openbox' ]; then
    if [ -f /usr/bin/tint2 ] || [ -f ~/.guix-profile/bin/tint2 ]; then
tint2 -c /home/teoten/Code/dot_tt/tint2/minima/minima.tint2rc &
    fi
fi

## Emacs at the end
if [ -f /usr/bin/emacs ] || [ -f ~/.guix-profile/bin/emacs ]; then
    emacs --daemon &
fi
