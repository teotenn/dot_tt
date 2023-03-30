# nmcli
alias wifi='nmcli dev wifi list'
alias wifi-conect='nmcli dev wifi connect'

# Screen config 
alias screen-my='xrandr --output LVDS-1 --mode 1280x800 --output VGA-1 --off'
alias screen-dual='xrandr --output LVDS-1 --mode 1280x800 --output VGA-1 --mode 1920x1080 --left-of LVDS-1'

# vm's
alias vm-antiX='cd ~/Code/VM; qemu-system-x86_64 -m 1024 -boot d -enable-kvm -smp 2 -net nic -net user -hda antiX.img'

# Conky
alias conky-run='killall conky; ~/.guix-profile/bin/conky -q -c ~/Code/dot_tt/conky/blue-white.conkyrc & ~/.guix-profile/bin/conky -q -c ~/Code/dot_tt/conky/clock.conkyrc & exit'

# Mounting stein
alias stein-open='sudo cryptsetup luksOpen /dev/sda2 sda2; sudo mount /dev/mapper/sda2 /mnt/stein'
alias stein-close='sudo umount /dev/mapper/sda2; sudo cryptsetup luksClose sda2'