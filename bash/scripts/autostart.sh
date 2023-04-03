thunar --daemon &
compton &
emacs --daemon &

sleep 2
/bin/bash /home/teoten/Code/dot_tt/bash/scripts/sirius_xrandr.sh

conky -q -c ~/Code/dot_tt/conky/blue-white.conkyrc & conky -q -c ~/Code/dot_tt/conky/clock.conkyrc &
tint2 -c ~/Code/dot_tt/tint2/minima/minima.tint2rc &
