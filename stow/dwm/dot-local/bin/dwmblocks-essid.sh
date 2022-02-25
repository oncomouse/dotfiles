#!/usr/bin/env bash
# Copyright (C) 2018 borgified <borgified@gmail.com>
# Copyright (C) 2014 Alexander Keller <github@nycroth.com>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#------------------------------------------------------------------------

INTERFACE="${INTERFACE:-wlan0}"

#------------------------------------------------------------------------

# As per #36 -- It is transparent: e.g. if the machine has no battery or wireless
# connection (think desktop), the corresponding block should not be displayed.
[[ ! -d /sys/class/net/${INTERFACE}/wireless ]] ||
    [[ "$(cat /sys/class/net/"$INTERFACE"/operstate)" = 'down' ]] && exit

#------------------------------------------------------------------------

ESSID=$(/sbin/iwconfig "$INTERFACE" | perl -n -e'/ESSID:"(.*?)"/ && print $1')

#------------------------------------------------------------------------

source ~/.cache/wal/colors.sh

case $BUTTON in
	1) /usr/bin/networkmanager_dmenu -fn "Hack-Regular:size=9" -nb "$color0" -nf "$color7" -sb "$color6" -sf "$color0";;
esac

echo -n "直 "
echo "$ESSID" # full text
# echo $ESSID # short text
