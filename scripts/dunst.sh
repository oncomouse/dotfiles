#!/bin/bash

source "${HOME}/.cache/wal/colors.sh"

/usr/bin/dunst \
  -lb "${color14:-#F0F0F0}" \
  -nb "${color4:-#F0F0F0}" \
  -cb "${color13:-#F0F0F0}" \
  -lf "${color0:=#000000}" \
  -cf "${color0:=#000000}" \
  -nf "${color0:=#000000}" \
  -li "" \
  -ni "" \
  -ci "" \
  -horizontal_padding 25
