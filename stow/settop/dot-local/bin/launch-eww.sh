#!/bin/bash

entity="$1"
verb="$2"
progressWindow="progressWindow"

progressWindow() {
    barIcon=""
    barPercent=""
    barCount=$(eww get barCount)
    if [ "$entity" == "volume" ]; then
        barPercent=$(pactl get-sink-volume @DEFAULT_SINK@ | head -n 1 | awk '{print substr($5, 1, length($5)-1)}')
        if [ "$verb" == "up" ]; then
            if [ "$barPercent" -lt 100 ]; then
                # pactl set-sink-volume @DEFAULT_SINK@ +5%
                barIcon="󰝝"
            else
                exit 1
            fi
        elif [ "$verb" == "down" ]; then
            # pactl set-sink-volume @DEFAULT_SINK@ -5%
            barIcon="󰝞"
        elif [ "$verb" == "mute" ]; then
            # pactl set-sink-mute @DEFAULT_SINK@ toggle
            barIcon="󰝟"
            if [ "$(pactl get-sink-mute @DEFAULT_SINK@ | cut --delimiter=" " -f 2)" == "no" ]; then
                barIcon="󰕾"
            fi
        else
            exit 1
        fi
        barPercent=$(pactl get-sink-volume @DEFAULT_SINK@ | head -n 1 | awk '{print substr($5, 1, length($5)-1)}')
    elif [ "$entity" == "brightness" ]; then
        if [ "$verb" == "up" ]; then
            brightnessctl set 5%+
        elif [ "$verb" == "down" ]; then
            brightnessctl set 5%-
        else
            exit 11
        fi
        barIcon="󰳲"
        barPercent=$(brightnessctl | sed -En 's/.*\(([0-9]+)%\).*/\1/p')
    fi

    eww update barIcon="$barIcon" barPercent="$barPercent" && eww open "$progressWindow"
    barCount=$((barCount + 1))
    eww update barCount="$barCount"

    sleep 1.5s

    barCount=$(eww get barCount)
    barCount=$((barCount - 1))
    eww update barCount="$barCount"
    if [ $barCount -lt 1 ]; then
        eww close "$progressWindow"
    fi
}

toggleOsd() {
    local file="$HOME/.cache/eww_launch.dashboard"
    if [[ ! -f "$file" ]]; then
        touch "$file"
        eww open-many osdBg osdWindow
    else
        rm "$file"
        eww close osdBg osdWindow
    fi
}

openWindow() {
    case "$entity" in
        volume|brightness)
            progressWindow
        ;;
        osd)
            toggleOsd
        ;;
    esac
}

openWindow
