#!/bin/sh

# Use the "logo" key as the primary modifier
mod="Mod4"

# Set your terminal emulator - foot
term=kitty

# App Bindings
# riverctl map normal $mod I spawn firefox
# riverctl map normal $mod O spawn brave-browser
riverctl map normal $mod+Mod1 R spawn "fuzzel -i Papirus -f 'Hasklug Nerd Font' --background=eeeeeeff -t 444444ff -w 60 --selection-color fcdbffff --selection-text-color 444444ff -T $term"

# Screenshot scripts
riverctl map normal "None" Print spawn $HOME/.local/bin/mygrimshot.sh
riverctl map normal "$mod" Print spawn '$HOME/.local/bin/mygrimshot.sh area'

# Mod+Shift+Return to start an instance of foot (https://codeberg.org/dnkl/foot)
riverctl map normal $mod+Shift Return spawn $term

# Mod+Q to close the focused view
riverctl map normal $mod+Shift C close

# Mod+E to exit river
riverctl map normal $mod+Shift Q exit

# Mod+J and Mod+K to focus the next/previous view in the layout stack
riverctl map normal $mod J focus-view next
riverctl map normal $mod K focus-view previous

# Mod+Shift+J and Mod+Shift+K to swap the focused view with the next/previous
# view in the layout stack
riverctl map normal $mod+Shift J swap next
riverctl map normal $mod+Shift K swap previous

# Mod+Period and Mod+Comma to focus the next/previous output
riverctl map normal $mod Period focus-output next
riverctl map normal $mod Comma focus-output previous

# Mod+Shift+{Period,Comma} to send the focused view to the next/previous output
riverctl map normal $mod+Shift Period send-to-output next
riverctl map normal $mod+Shift Comma send-to-output previous

# Mod+Return to bump the focused view to the top of the layout stack
riverctl map normal $mod Return zoom

# Mod+H and Mod+L to decrease/increase the main ratio of rivercarro
riverctl map normal $mod H send-layout-cmd rivercarro "main-ratio -0.05"
riverctl map normal $mod L send-layout-cmd rivercarro "main-ratio +0.05"

# Mod+Shift+H and Mod+Shift+L to increment/decrement the main_count value of rivertile.
riverctl map normal $mod+Shift H mod-layout-value rivercarro "main_count +1"
riverctl map normal $mod+Shift L mod-layout-value rivercarro "main_count -1"

# Mod+Alt+{H,J,K,L} to move views
riverctl map normal $mod+Mod1 H move left 100
riverctl map normal $mod+Mod1 J move down 100
riverctl map normal $mod+Mod1 K move up 100
riverctl map normal $mod+Mod1 L move right 100

# Mod+Alt+Control+{H,J,K,L} to snap views to screen edges
riverctl map normal $mod+Mod1+Control H snap left
riverctl map normal $mod+Mod1+Control J snap down
riverctl map normal $mod+Mod1+Control K snap up
riverctl map normal $mod+Mod1+Control L snap right

# Mod+Alt+Shif+{H,J,K,L} to resize views
riverctl map normal $mod+Mod1+Shift H resize horizontal -100
riverctl map normal $mod+Mod1+Shift J resize vertical 100
riverctl map normal $mod+Mod1+Shift K resize vertical -100
riverctl map normal $mod+Mod1+Shift L resize horizontal 100

# Mod + Left Mouse Button to move views
riverctl map-pointer normal $mod BTN_LEFT move-view

# Mod + Right Mouse Button to resize views
riverctl map-pointer normal $mod BTN_RIGHT resize-view

for i in $(seq 1 9)
do
    tags=$((1 << ($i - 1))) # Ask ifreund why he does this. It makes sense though.

    # Mod+[1-9] to focus tag [0-8]
    riverctl map normal $mod $i set-focused-tags $tags

    # Mod+Shift+[1-9] to tag focused view with tag and also move window with it. [0-8]
    # I made it like this because I want to move the stuff and the views at the same time. weird hack
    riverctl map normal $mod+Shift $i spawn "riverctl set-view-tags $tags; riverctl set-focused-tags $tags"  

    # Mod+Ctrl+[1-9] to toggle focus of tag [0-8]
    riverctl map normal $mod+Control $i toggle-focused-tags $tags

    # Mod+Shift+Ctrl+[1-9] to toggle tag [0-8] of focused view
    riverctl map normal $mod+Shift+Control $i toggle-view-tags $tags
done

# Mod+0 to focus all tags
# Mod+Shift+0 to tag focused view with all tags
all_tags=$(((1 << 32) - 1))
riverctl map normal $mod 0 set-focused-tags $all_tags
riverctl map normal $mod+Shift 0 set-view-tags $all_tags

# Mod+Space to toggle float
riverctl map normal $mod Space toggle-float

# Mod+F to toggle fullscreen
riverctl map normal $mod F toggle-fullscreen

# Mod+{Up,Right,Down,Left} to change layout orientation
riverctl map normal $mod Up    send-layout-cmd rivercarro "main-location top"
riverctl map normal $mod Right send-layout-cmd rivercarro "main-location right"
riverctl map normal $mod Down  send-layout-cmd rivercarro "main-location bottom"
riverctl map normal $mod Left  send-layout-cmd rivercarro "main-location left"

# And for monocle
riverctl map normal $mod M     send-layout-cmd rivercarro "main-location monocle"

# Borders
riverctl border-color-focused 0x444444ff
riverctl border-color-unfocused 0xf9e0f4ff
riverctl border-width 2
riverctl background-color 0x1d2021

# Declare a passthrough mode. This mode has only a single mapping to return to
# normal mode. This makes it useful for testing a nested wayland compositor
riverctl declare-mode passthrough

# Mod+F11 to enter passthrough mode
riverctl map normal $mod F11 enter-mode passthrough

# Mod+F11 to return to normal mode
riverctl map passthrough $mod F11 enter-mode normal

# Various media key mapping examples for both normal and locked mode which do
# not have a modifier
for mode in normal locked
do
    # Eject the optical drive
    riverctl map $mode None XF86Eject spawn eject -T

done

# Control pulse audio volume with pamixer (https://github.com/cdemoulins/pamixer)
riverctl map normal None XF86AudioRaiseVolume  spawn 'pamixer -i 5'
riverctl map normal None XF86AudioLowerVolume  spawn 'pamixer -d 5'
riverctl map normal None XF86AudioMute         spawn 'pamixer --toggle-mute'

# Control MPRIS aware media players with playerctl (https://github.com/altdesktop/playerctl)
riverctl map normal None XF86AudioMedia spawn 'playerctl -p spotify play-pause'
riverctl map normal None XF86AudioPlay  spawn 'playerctl -p spotify play-pause'
riverctl map normal None XF86AudioPrev  spawn 'playerctl -p spotify previous'
riverctl map normal None XF86AudioNext  spawn 'playerctl -p spotify next'

# Control screen backlight brighness with light (https://github.com/haikarainen/light)
riverctl map normal None XF86MonBrightnessUp   spawn 'brightnessctl s +1%'
riverctl map normal None XF86MonBrightnessDown spawn 'brightnessctl s 1%-'

# Set repeat rate
riverctl set-repeat 50 300

# Set app-ids of views which should float
riverctl float-filter-add "gedit"

# Set app-ids of views which should use client side decorations
riverctl csd-filter-add "gedit"

# Set opacity and fade effect
riverctl opacity 1.0 1.0 0.50 0.3 20

riverctl default-layout rivercarro
exec rivercarro

# River will send the process group of the init executable SIGTERM on exit.

