#!/bin/bash
shopt -s nocasematch #sets case insensitive option for the shell

websites=("youtube" "pbs")

__activate_tridactyl() {
	xdotool key ctrl+comma
	sleep 0.5s
}

helper_message() {
	sdorfehs -c "echo ${1}"
}

helper_volume() {
	__bluetooth() {
		local method
		[[ $1 == up ]] && method=VolumeUp || method=VolumeDown
		dbus-send \
			--print-reply --type=method_call \
			--system --dest=org.bluez \
			/org/bluez/hci0/dev_"$2" org.bluez.MediaControl1."$method" >/dev/null
	}

	__pulseaudio() {
		local switch
		if [[ $(pamixer --get-volume-human) == muted ]]; then
			pamixer -t
		fi
		[[ $1 == up ]] && switch="-i" || switch="-d"
		pamixer "$switch" 5
		pamixer --get-volume-human >>/tmp/xobpipe
	}
	if [[ ${1-} != @(up|down) ]]; then
		echo 'up|down expected'
		exit 1
	fi

	if [[ $(pactl info) =~ "Default Sink: bluez_sink."([0-9A-F_]*)".a2dp_sink" ]]; then
		bt_addr="${BASH_REMATCH[1]}"
		__bluetooth "$1" "$bt_addr" && exit 0
	fi

	__pulseaudio "$1"
}

helper_find_or_open() {
	windows=$(wmctrl -l)
	if [[ "$windows" == *"$1"* ]]; then
		xdotool windowactivate "$(wmctrl -l | grep -i "$1" | cut -d " " -f 1)"
	else
		nohup "$1" >/dev/null 2>&1 &
	fi
}

action_focus() {
	helper_find_or_open "$1"
	pkill flex-launcher 2> /dev/null
}

action_prev() {
	# helper_message "called: prev"
	case "$1" in
	*kodi*)
		kodi-send --action="StepBack"
		;;
	*freetube*)
		sleep 0.2 &&
			xdotool key --clearmodifiers "j"
		;;
		# *)
		#     helper_message "no action taken for $1"
		#     ;;
	esac
}

action_next() {
	# helper_message "called: next"
	case "$1" in
	*kodi*)
		kodi-send --action="StepForward"
		;;
	*freetube*)
		sleep 0.2 &&
			xdotool key --clearmodifiers "l"
		;;
		# *)
		#     helper_message "no action taken for $target"
		#     ;;
	esac
}

action_play() {
	# helper_message "called: play"
	case "$1" in
	*kodi*)
		kodi-send --action="Pause"
		;;
	*freetube*)
		sleep 0.2 &&
			xdotool key --clearmodifiers "space"
		;;
		# *)
		#     helper_message "no action taken for $1"
		#     ;;
	esac
}

__muted=0

helper_feed_xob() {
	pactl get-sink-volume @DEFAULT_SINK@ | head -n 1 | awk '{print substr($5, 1, length($5)-1)}' >>/tmp/xobpipe
}

action_vol_up() {
	helper_volume up
	helper_feed_xob
}

action_vol_down() {
	helper_volume down
	helper_feed_xob
}

action_main() {
	helper_find_or_open flex-launcher
}

action_notification() {
	helper_find_or_open flex-launcher
}

action_search() {
	# helper_message "called: search"
	case "$1" in
	*kodi*)
		kodi-send --action="VideoLibrary.Search"
		;;
	*freetube*)
		sleep 0.2s &&
			xdotool key --clearmodifiers ctrl+l
		;;
	esac
}

# Subtitles
action_settings() {
	# helper_find_or_open flex-launcher
	# helper_message "called: settings"
	case "$1" in
	*kodi*)
		kodi-send --action="ShowSubtitles"
		;;
	*freetube*)
		sleep 0.2 &&
			xdotool key --clearmodifiers c
		;;
	esac
}

action_action_center() {
	helper_find_or_open flex-launcher
}

action_file_explorer() {
	helper_find_or_open flex-launcher
}

# Home
action_desktop() {
	# helper_message "called: desktop"
	case "$1" in
	*firefox*)
		xdg-open "https://www.youtube.com" &&
			sleep 0.1s &&
			xdotool key --clearmodifiers ctrl+Prior ctrl+w
		;;
	*kodi*)
		kodi-send --action="PreviousMenu"
		;;
	*freetube*)
		xdotool mousemove 25 100 click 1
		;;
	esac
}

# Playlist
action_tasks() {
	helper_find_or_open flex-launcher
}

# Fullscreen
action_split_up() {
	# helper_message "called: split_up"
	case "$1" in
	*freetube*)
		sleep 0.2s &&
			xdotool key --clearmodifiers f
		;;
	*firefox_youtube*)
		xdotool key --clearmodifiers ctrl+comma sleep 0.75 key colon sleep 0.1 &&
			xdotool type --delay 0.25 --clearmodifiers "hint -Jc .ytp-fullscreen-button" &&
			xdotool key --clearmodifiers Return
		;;
	esac
}

action_split_down() {
	helper_find_or_open flex-launcher
}

action_split_left() {
	helper_find_or_open flex-launcher
}

action_split_right() {
	helper_find_or_open flex-launcher
}

action_refresh() {
	helper_find_or_open flex-launcher
}

action_close() {
	helper_find_or_open flex-launcher
	# # helper_message "called: close"
	# case "$1" in
	# kodi*)
	# 	sleep 0.2s &&
	# 		xdotool key -clearmodifiers BackSpace
	# 	;;
	# firefox*)
	# 	if [[ "$1" == *"youtube"* ]]; then
	# 		xdg-open "https://www.youtube.com"
	# 		sleep 0.1s
	# 		xdotool key --clearmodifiers ctrl+Prior ctrl+w
	# 	else
	# 		xdotool key --clearmodifiers ctrl+t ctrl+Prior ctrl+w
	# 	fi
	# 	;;
	# esac
	# return
}

action_browser() {
	helper_find_or_open flex-launcher
}

action_task_manager() {
	helper_find_or_open flex-launcher
}

# Using as "stop"
action_move_up() {
	# helper_message "called: move_up"
	case "$1" in
	*kodi*)
		kodi-send --action="Stop"
		;;
	*freetube*)
		sleep 0.2s &&
			xdotool key --clearmodifiers "space"
		;;
	esac
}

action_move_down() {
	helper_message "called: move_down"
}

action_move_left() {
	helper_message "called: move_left"
}

action_move_right() {
	helper_message "called: move_right"
}

target="$(xdotool getactivewindow getwindowclassname)"

if [[ "$target" == *"firefox"* ]]; then
	name="$(xdotool getactivewindow getwindowname)"
	for ws in "${websites[@]}"; do
		if [[ "$name" == *"$ws"* ]]; then
			target+="_$ws"
		fi
	done
fi

printf "action: %s\ntarget: %s\n" "$1" "$target"

if [[ $(type -t "action_${1-}") == function ]]; then
	# Focus takes a second application
	if [[ "$1" == "focus" ]]; then
		target="$2"
	fi
	"action_$1" "${target}"
else
	echo "unexpected action: ${1-}"
fi
# vim:ft=sh
