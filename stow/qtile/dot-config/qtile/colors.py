import json
import os

XDG_CACHE_HOME = (
    os.environ["XDG_CACHE_HOME"]
    if "XDG_CACHE_HOME" in os.environ
    else os.path.join(os.environ["HOME"], ".config")
)
pywal_cache_file = os.path.join(XDG_CACHE_HOME, "wal", "colors.json")

if os.path.isfile(pywal_cache_file):
    with open(pywal_cache_file) as f:
        colorscheme = json.load(f)
else:
    colorscheme = dict(
        special=dict(foreground="#aeadaf", background="#232323", cursor="#aeadaf"),
        colors=dict(
            color0="#232323",
            color1="#d2813d",
            color2="#8c9e3d",
            color3="#b1942b",
            color4="#6e9cb0",
            color5="#b58d88",
            color6="#6da280",
            color7="#949d9f",
            color8="#312e30",
            color9="#d0913d",
            color10="#96a42d",
            color11="#a8a030",
            color12="#8e9cc0",
            color13="#d58888",
            color14="#7aa880",
            color15="#aeadaf",
        ),
    )

colors = dict(
    layout_border=colorscheme["colors"]["color4"],
    bg=colorscheme["colors"]["color8"],
    rofi=colorscheme["colors"]["color7"],
    groups_bg=colorscheme["colors"]["color4"],
    groups_active=colorscheme["colors"]["color7"],
    groups_highlight=colorscheme["colors"]["color12"],
    groups_highlight_color=colorscheme["colors"]["color7"],
    groups_inactive=colorscheme["colors"]["color0"],
    tasklist_bg=colorscheme["colors"]["color8"],
    sep=colorscheme["colors"]["color7"],
    player_bg=colorscheme["colors"]["color4"],
    lang_bg=colorscheme["colors"]["color4"],
    battery_bg=colorscheme["colors"]["color4"],
    volume_bg=colorscheme["colors"]["color4"],
    clock_bg=colorscheme["colors"]["color4"],
    clock_fg=colorscheme["colors"]["color7"],
    layout_icon_bg=colorscheme["colors"]["color7"],
)
