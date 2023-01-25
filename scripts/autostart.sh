#!/bin/bash

# Depending on the n monitors connected, setup certain parameters
# Uses xwallpaper and xrandr, make sure to install it
function n_mons(){
    local nmons=()
    local monitor
    local PAPESDIR=$HOME/Pictures/wallpapers
    local PAPE=$(find "$PAPESDIR" -type f | sort -R | tail -1)
    for monitor in $(xrandr | awk '/ connected/ && /[[:digit:]]x[[:digit:]].*+/{print $1}'); do
	nmons+=("$monitor")
    done
    if [ "${#nmons[@]}" -eq 1 ]
    then
	xrandr --output LVDS-1 --mode 1280x800 --output VGA-1 --off
	xwallpaper --output LVDS-1 --center "$PAPE"
    else
	local PAPEB=$(find "$PAPESDIR" -type f | sort -R | tail -1)
	xrandr --output LVDS-1 --mode 1280x800 --output VGA-1 --mode 1920x1080 --left-of LVDS-1
	xwallpaper --output LVDS-1 --center "$PAPE" --output VGA-1 --zoom "$PAPEB"
    fi
}

n_mons

