import os

from groups import groups
from keys import keys
from libqtile import hook
from libqtile import layout
from libqtile.command import lazy
from libqtile.config import Group
from mouse import mouse
from screen import screens

layouts = [
    layout.Bsp,
]

widget_defaults = {"font": "FiraCode Nerd Font", "fontsize": 12, "padding": 3}


def wallpaper():
    path = "~/dotfiles/conf/wallpaper/wallhaven-453lo1.jpg"
    os.system("feh --bg-scale " + path)


@hook.subscribe.startup
def autostart():
    wallpaper()


dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = False
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating()
auto_fullscreen = True
focus_on_window_activation = "focus"

wmname = "qtile"
