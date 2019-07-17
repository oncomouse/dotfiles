function fish_right_prompt
  if [ -z $TMUX ]
    battery.info.update
    set -l battery_value (math $BATTERY_PCT + 4.5)

    set_color 65737E
    echo -n \ue0b2
    set_color -b 65737E 343D46
    set -q BATTERY_IS_PLUGGED; and echo -n \ufba3; or echo -n \uf583
    echo " "
    if command -sq node
      set_color (node ~/dotfiles/tools/color-gradient-picker.js "[[0,[215,0,0]],[20,[255,95,0]],[100,[95,255,0]]]" $BATTERY_PCT | tr -d "#")
    else
      if [ "$battery_value" -gt 50 ]
        set_color 5fff00
      else if [ "$battery_value" -gt 20 ]
        set_color ff5f00
      else
        set_color d70000
      end
    end
    echo $battery_value | spark --max=100
    echo " "
    set_color -b 65737E blue
    echo -n \ue0b2
    set_color -b blue white
    echo -n " "
    date "+%X"
    set_color normal
  end
end

