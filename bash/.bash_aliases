# nmcli
alias wifi='nmcli dev wifi list'
alias wifi-conectar='nmcli dev wifi connect'

# Monitor
alias screen-my='xrandr --output LVDS-1 --mode 1280x800 --output VGA-1 --off; nitrogen --restore'
# Next dual screen for i3 desk mainly
alias screen-dual='xrandr --output LVDS-1 --mode 1280x800 --output VGA-1 --mode 1920x1080 --left-of LVDS-1; nitrogen --restore'

# git for personal
alias personal-pull='cd ~/Code/personal_config; git checkout main; git pull'
alias personal-merge='git checkout debian; git merge main'

# vm's
alias vm-antiX='cd ~/Code/VM; qemu-system-x86_64 -m 1024 -boot d -enable-kvm -smp 2 -net nic -net user -hda antiX.img'