#!/usr/bin/env bash

## Author  : Aditya Shakya
## Mail    : adi1090x@gmail.com
## Github  : @adi1090x
## Twitter : @adi1090x

## Modified: Andrew Pilsch
## Mail    : oncomouse@gmail.com
## Github  : @oncomouse

# Available Styles
# >> Created and tested on : rofi 1.6.0-1
#
# column_circle     column_square     column_rounded     column_alt
# card_circle     card_square     card_rounded     card_alt
# dock_circle     dock_square     dock_rounded     dock_alt
# drop_circle     drop_square     drop_rounded     drop_alt
# full_circle     full_square     full_rounded     full_alt
# row_circle      row_square      row_rounded      row_alt

theme="full_square"
dir="$HOME/dotfiles/conf/rofi/powermenu"

# color="\~\/.cache\/wal\/colors-powermenu"
# comment this line to disable random colors
# sed -i -e "s/@import .*/@import \"$color\"/g" "$dir"/styles/colors.rasi

uptime=$(uptime -p | sed -e 's/up //g')

rofi_command="rofi -theme $dir/$theme"

# Pause music:
pause_command="playerctl pause"
# Mute all sound:
mute_command="ponymix mute"

base_command () {
  basename "$(echo "$1" | cut -d " " -f1)"
}

run_command_if_found () {
  eval "$1"
  local cmd
  cmd=$(base_command "$1")
  if command -v "$cmd"; then
    eval "$1"
  fi
}

pause_music () {
  run_command_if_found "$pause_command"
}

mute () {
  run_command_if_found "$mute_command"
}

# Options
shutdown="襤"
reboot="勒"
lock=""
suspend="鈴"
logout=""

# Confirmation
confirm_exit() {
  rofi -dmenu\
    -i\
    -no-fixed-num-lines\
    -p "Are You Sure? : "\
    -theme "$dir"/confirm.rasi \
    -font "FiraCode Nerd Font 16"
}

# Message
msg() {
  rofi -theme "$dir/message.rasi" -e "Available Options  -  yes / y / no / n"
}

# Variable passed to rofi
options="$shutdown\n$reboot\n$lock\n$suspend\n$logout"

chosen="$(echo -e "$options" | $rofi_command -p "Uptime: $uptime" -dmenu -selected-row 2)"
case $chosen in
  "$shutdown")
    ans=$(confirm_exit &)
    if [[ $ans == "yes" || $ans == "YES" || $ans == "y" || $ans == "Y" ]]; then
      systemctl poweroff
    elif [[ $ans == "no" || $ans == "NO" || $ans == "n" || $ans == "N" ]]; then
      exit 0
    else
      msg
    fi
    ;;
  "$reboot")
    ans=$(confirm_exit &)
    if [[ $ans == "yes" || $ans == "YES" || $ans == "y" || $ans == "Y" ]]; then
      systemctl reboot
    elif [[ $ans == "no" || $ans == "NO" || $ans == "n" || $ans == "N" ]]; then
      exit 0
    else
      msg
    fi
    ;;
  "$lock")
    pause_music
    mute
    xscreensaver-command -lock
    ;;
  "$suspend")
    ans=$(confirm_exit &)
    if [[ $ans == "yes" || $ans == "YES" || $ans == "y" || $ans == "Y" ]]; then
      pause_music
      mute
      # systemctl suspend
      xscreensaver-command -lock
      xset dpms force off
    elif [[ $ans == "no" || $ans == "NO" || $ans == "n" || $ans == "N" ]]; then
      exit 0
    else
      msg
    fi
    ;;
  "$logout")
    ans=$(confirm_exit &)
    if [[ $ans == "yes" || $ans == "YES" || $ans == "y" || $ans == "Y" ]]; then
      echo "awesome.quit()" | awesome-command
      # if [[ "$DESKTOP_SESSION" == "Openbox" ]]; then
      #   openbox --exit
      # elif [[ "$DESKTOP_SESSION" == "bspwm" ]]; then
      #   bspc quit
      # elif [[ "$DESKTOP_SESSION" == "i3" ]]; then
      #   i3-msg exit
      # fi
    elif [[ $ans == "no" || $ans == "NO" || $ans == "n" || $ans == "N" ]]; then
      exit 0
    else
      msg
    fi
    ;;
esac
