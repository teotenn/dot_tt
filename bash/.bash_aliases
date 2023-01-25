# nmcli
alias wifi='nmcli dev wifi list'
alias wifi-conectar='nmcli dev wifi connect'

# Screen config 
alias screen-my='xrandr --output LVDS-1 --mode 1280x800 --output VGA-1 --off; nitrogen --restore'
alias screen-dual='xrandr --output LVDS-1 --mode 1280x800 --output VGA-1 --mode 1920x1080 --left-of LVDS-1; nitrogen --restore'

# vm's
alias vm-antiX='cd ~/Code/VM; qemu-system-x86_64 -m 1024 -boot d -enable-kvm -smp 2 -net nic -net user -hda antiX.img'

# Conky
alias conky-run='killall conky; ~/.guix-profile/bin/conky -q -c ~/Code/dot_tt/conky/blue-white.conkyrc & ~/.guix-profile/bin/conky -q -c ~/Code/dot_tt/conky/clock.conkyrc & exit'

# Starship init
alias sship='eval "$(starship init bash)"'