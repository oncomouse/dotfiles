function battery-status
  battery.info.update
  if [ "$BATTERY_PCT" -gt 50 ]
    set_color 5fff00
  else if [ "$BATTERY_PCT" -gt 20 ]
    set_color ff5f00
  else
    set_color d70000
  end
  if [ "$BATTERY_PCT" -ge 96 ]
    set -q BATTERY_IS_PLUGGED; and echo \uf584; or echo \uf578
  else if set -q BATTERY_IS_PLUGGED
    if [ "$BATTERY_PCT" -ge 90 ]
      echo \uf58a
    else if [ "$BATTERY_PCT" -ge 80 ]
      echo \uf589
    else if [ "$BATTERY_PCT" -ge 60 ]
      echo \uf588
    else if [ "$BATTERY_PCT" -ge 40 ]
      echo \uf587
    else if [ "$BATTERY_PCT" -ge 30 ]
      echo \uf586
    else
      echo \uf585
    end
  else
    if [ "$BATTERY_PCT" -ge 90 ]
      echo \uf581
    else if [ "$BATTERY_PCT" -ge 80 ]
      echo \uf580
    else if [ "$BATTERY_PCT" -ge 70 ]
      echo \uf57f
    else if [ "$BATTERY_PCT" -ge 60 ]
      echo \uf57e
    else if [ "$BATTERY_PCT" -ge 50 ]
      echo \uf57d
    else if [ "$BATTERY_PCT" -ge 40 ]
      echo \uf57c
    else if [ "$BATTERY_PCT" -ge 30 ]
      echo \uf57b
    else if [ "$BATTERY_PCT" -ge 20 ]
      echo \uf57a
    else
      echo \uf579
    end
  end
  set_color normal
end
