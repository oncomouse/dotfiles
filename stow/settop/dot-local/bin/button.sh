#!/usr/bin/env bash

__activate_tridactyl() {
    xdotool key ctrl+comma
    sleep 0.5s
}

action_prev() {
    return
}

action_next() {
    return
}

action_play() {
    return
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
        pamixer --get-volume-human >> /tmp/xobpipe
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

action_vol_up() {
    helper_volume up
}

action_vol_down() {
    helper_volume down
}

action_main() {
    return
}

action_notification() {
    return
}

action_search() {
    if [[ "$target" == "firefox" ]]; then
        xdotool key Escape colon sleep 0.1 &&
            xdotool type --delay 15 "focusinput" &&
            xdotool key Return
    fi
    return
}

action_settings() {
    return
}

action_action_center() {
    return
}

action_file_explorer() {
    return
}

action_desktop() {
    return
}

action_tasks() {
    return
}

action_split_up() {
    return
}

action_split_down() {
    return
}

action_split_left() {
    return
}

action_split_right() {
    return
}

action_split_refresh() {
    if [[ "$target" == "firefox" ]]; then
        xdotool key --clearmodifiers ctrl+r
    fi
    return
}

action_split_close() {
    return
}

action_browser() {
    firefox --new-window "https://www.youtube.com" &
    disown
    xdotool search --sync --limit 1 --name "Mozilla Firefox" windowclose
    return
}

action_task_manager() {
    return
}

action_move_up() {
    return
}

action_move_down() {
    return
}

action_move_left() {
    return
}

action_move_right() {
    return
}

target="$(xdotool getwindowclassname "$(xdotool getactivewindow)")"

if [[ $(type -t "action_${1-}") == function ]]; then
    "action_$1" "${target}"
else
    echo "unexpected action: ${1-}"
fi
# vim:ft=sh

# xdotool search "Mozilla Firefox" windowactivate --sync key --clearmodifiers ctrl+l key Delete type "https://youtube.com" && xdotool key Return
