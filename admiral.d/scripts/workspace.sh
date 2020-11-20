#!/bin/bash
# Source: https://raw.githubusercontent.com/kbrgl/dotfiles/2b3d4078310dc265abcc31714f58243c11834d02/bin/workspaces.sh

# '#232323','#d2813d','#8c9e3d','#b1942b','#6e9cb0','#b58d88','#6da280','#949d9f','#312e30','#d0913d','#96a42d','#a8a030','#8e9cc0','#d58888','#7aa880','#aeadaf' 
o="\uf111"
c="\uf192"
free_color='#949D9F'
occupied_color='#6da280'
alert_color='#d2813d'

chds() {
    echo -en "%{A:bspc desktop -f $1:}$2%{A} "
}

while read -r line; do
    echo -n "%{A4:bspc desktop -f prev:}%{A5:bspc desktop -f next:}"
    echo -n " "
    case $line in
        W*)
            IFS=':'
            set -- ${line#?}
            while [ $# -gt 0 ]; do
                item="$1"
                name="${item#?}"
                case $item in
                    f*)
                        # free desktop
                        echo -n "%{F$free_color}"
                        chds $name $o
                        echo -n "%{F-}"
                        ;;
                    F*)
                        # focused free desktop
                        echo -n "%{F$free_color}"
                        chds $name $c
                        echo -n "%{F-}"
                        ;;
                    o*)
                        # occupied desktop
                        echo -n "%{F$occupied_color}"
                        chds $name $o
                        echo -n "%{F-}"
                        ;;
                    O*)
                        # focused occupied desktop
                        echo -n "%{F$occupied_color}"
                        chds $name $c
                        echo -n "%{F-}"
                        ;;
                    u*)
                        # urgent desktop
                        echo -n "%{F$alert_color}"
                        chds $name $o
                        echo -n "%{F-}"
                        ;;
                    U*)
                        # focused urgent desktop
                        echo "%{F#6da280}"
                        chds $name $c
                        echo "%{F-}"
                        ;;
                esac
                shift
            done
            ;;
    esac
    echo -n "%{A}%{A}"
    echo
done < <(bspc subscribe report)
