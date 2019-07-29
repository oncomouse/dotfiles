function kitty-colors
  set colors (ag --nobreak color[0-9] ~/.config/kitty/kitty.conf --vimgrep | cut -d: -f4 | string replace -r "color[0-9]+\s+" ""| string join "," | string replace -a "#" "'#" | string replace -a "," "',"); echo -n "["$colors"']"
end
