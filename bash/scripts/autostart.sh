thunar --daemon &

if [ -f /usr/bin/compton ] || [ -f ~/.guix-profile/bin/compton ]; then
    compton &
fi

emacs --daemon &

sleep 2
if [ -f /home/teoten/Code/dot_tt/bash/scripts/sirius_xrandr.sh ]; then
    bash /home/teoten/Code/dot_tt/bash/scripts/sirius_xrandr.sh
fi

conky -q -c ~/Code/dot_tt/conky/blue-white.conkyrc & conky -q -c ~/Code/dot_tt/conky/clock.conkyrc &
tint2 -c ~/Code/dot_tt/tint2/minima/minima.tint2rc &
