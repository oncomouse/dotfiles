#!/bin/sh

# Use the "logo" key as the primary modifier
mod="Mod4"

# Set your terminal emulator - foot
term=kitty

# Launcher Group {{{
riverctl map normal $mod+Mod1 R spawn "wofi -I --show drun --prompt=Applications --height=750 --width=600 --term=$term"
# riverctl map normal $mod+Shift N spawn $rofinetworkcmd
# riverctl map normal $mod+Control Space $rofiemojicmd
riverctl map normal $mod+Shift W spawn "wofi -I --show windows"
# riverctl map normal $mod+Shift P spawn $powermenu
# }}}
# River Group {{{
riverctl map normal $mod+Shift Return spawn $term # Open a Terminal
# riverctl map normal $mod+SHift B spawn "dwm-brightness.sh default; eww update" # Set Default Brightness
#riverctl map normal $mod B # Toggle Bar Visibility
#riverctl map normal $mod Q # Reload River
riverctl map normal $mod+Shift Q exit # Quit River
# }}}
# Client Group {{{
riverctl map normal $mod J focus-view next # Focus Next by Index
riverctl map normal $mod K focus-view previous # Focus Previous by Index
riverctl map normal $mod H send-layout-cmd stacktile "primary_ratio -0.05" # Increase Primary Width Factor
riverctl map normal $mod L send-layout-cmd stacktile "primary_ratio +0.05" # Decrease Primary Width Factor
riverctl map normal $mod I send-layout-cmd stacktile "primary_count -1" # Increase the Number of Primary Clients
riverctl map normal $mod D send-layout-cmd stacktile "primary_count +1" # Decrease the Number of Primary Clients
riverctl map normal $mod+Shift J swap next # Swap Client with Next by Index
riverctl map normal $mod+Shift K swap previous # Swap Client with Previous by Index
riverctl map normal $mod Tab focus-previous-tags
# riverctl map normal $mod U # Jump to Urgent Client
riverctl map normal $mod Return zoom # Zoom
riverctl map normal $mod+Shift C close # Close Client
riverctl map normal $mod Space toggle-float # Toggle Floating
riverctl map normal $mod Left move left 25
riverctl map normal $mod Down move down 25
riverctl map normal $mod Up move up 25
riverctl map normal $mod Right move right 25
riverctl map normal $mod+Shift Left resize horizontal -25
riverctl map normal $mod+Shift Down resize vertical 25
riverctl map normal $mod+Shift Up resize vertical -25
riverctl map normal $mod+Shift Right resize horizontal 25
riverctl map normal $mod+Control Left snap left
riverctl map normal $mod+Control Down snap down
riverctl map normal $mod+Control Up snap up
riverctl map normal $mod+Control Right snap right
# }}}
# Layout Group {{{
riverctl map normal $mod T spawn "$HOME/.config/river/master_stack.sh" # Select Tile Layout
riverctl map normal $mod+Shift M spawn "$HOME/.config/river/monocle.sh" # Select Monocle Layout
# riverctl map normal $mod M "$HOME/.config/river/centered_monocle.sh" # Select Centered Monocle Layout
# riverctl map normal $mod+Shift T "$HOME/.config/river/rtile.sh" # Select Right Tile Layout
# }}}
# Tag Group {{{
for i in $(seq 1 9)
do
	tags=$((1 << ($i - 1))) # Ask ifreund why he does this. It makes sense though.

	# Mod+[1-9] to focus tag [0-8]
	riverctl map normal $mod "$i" set-focused-tags $tags

	# Mod+Shift+[1-9] to tag focused view with tag and also move window with it. [0-8]
	# I made it like this because I want to move the stuff and the views at the same time. weird hack
	riverctl map normal $mod+Shift "$i" spawn "riverctl set-view-tags $tags; riverctl set-focused-tags $tags"  

	# Mod+Ctrl+[1-9] to toggle focus of tag [0-8]
	riverctl map normal $mod+Control "$i" toggle-focused-tags $tags

	# Mod+Shift+Ctrl+[1-9] to toggle tag [0-8] of focused view
	riverctl map normal $mod+Shift+Control "$i" toggle-view-tags $tags
done
# }}}
# Media Group {{{
riverctl map normal None XF86KbdBrightnessUp spawn "sudo /usr/local/bin/keyboard-backlight up"
riverctl map normal None XF86KbdBrightnessDown spawn "sudo /usr/local/bin/keyboard-backlight down"
riverctl map normal None XF86MonBrightnessUp   spawn "dwm-brightness up"
riverctl map normal None XF86MonBrightnessDown spawn "dwm-brightness down"
riverctl map normal None XF86AudioRaiseVolume  spawn "liskin-media up"
riverctl map normal None XF86AudioLowerVolume  spawn "liskin-media down"
riverctl map normal None XF86AudioMute         spawn "liskin-media mute"
riverctl map normal None XF86AudioPlay  spawn "liskin-media play"
riverctl map normal None XF86AudioPrev  spawn "liskin-media prev"
riverctl map normal None XF86AudioNext  spawn "liskin-media next"
riverctl map normal None XF86AudioStop  spawn "liskin-media stop"
riverctl map normal "None" Print spawn "$HOME/.local/bin/mygrimshot.sh"
riverctl map normal "$mod" Print spawn "$HOME/.local/bin/mygrimshot.sh area"
for mode in normal locked; do
	riverctl map $mode None XF86Eject spawn eject -T # Eject CD-ROM
done
# }}}
# Mouse Bindings{{{
riverctl map-pointer normal $mod BTN_LEFT move-view
riverctl map-pointer normal $mod BTN_RIGHT resize-view
# }}}
# Mod+Period and Mod+Comma to focus the next/previous output
riverctl map normal $mod Period focus-output next
riverctl map normal $mod Comma focus-output previous

# Mod+Shift+{Period,Comma} to send the focused view to the next/previous output
riverctl map normal $mod+Shift Period send-to-output next
riverctl map normal $mod+Shift Comma send-to-output previous

# Mod+F to toggle fullscreen
riverctl map normal $mod F toggle-fullscreen

# Declare a passthrough mode. This mode has only a single mapping to return to
# normal mode. This makes it useful for testing a nested wayland compositor
riverctl declare-mode passthrough
# Mod+F11 to enter passthrough mode
riverctl map normal $mod F11 enter-mode passthrough
# Mod+F11 to return to normal mode
riverctl map passthrough $mod F11 enter-mode normal

# Various media key mapping examples for both normal and locked mode which do
# not have a modifier

# Control pulse audio volume with pamixer (https://github.com/cdemoulins/pamixer)

# Set repeat rate
riverctl set-repeat 50 300

# Set app-ids of views which should float
riverctl float-filter-add "mpv"
# Set app-ids of views which should use client side decorations
# riverctl csd-filter-add "gedit"

# Set opacity and fade effect
# riverctl opacity 1.0 1.0 0.50 0.3 20

riverctl default-layout stacktile

riverctl spawn "stacktile --per-tag-config --primary-count 1  --secondary-count 0 --primary-sublayout rows --primary-position left --primary-ratio 0.65 --outer-padding 0 --inner-padding 0 --secondary-sublayout rows --secondary-ratio 0.5 --remainder-sublayout rows" 
riverctl default-layout stacktile
# River will send the process group of the init executable SIGTERM on exit.

# vim:foldmethod=marker:foldlevel=0
