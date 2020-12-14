# type: ignore
from typing import List  # noqa: F401

from groups import groups
from hooks import *
from keys import keys
from keys import mouse
from layouts import floating_layout
from layouts import layouts
from screens import extension_defaults
from screens import screens
from screens import widget_defaults

# Custom Imports

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None  # WARNING: this is deprecated and will be removed soon
follow_mouse_focus = True
bring_front_click = True
cursor_warp = False

auto_fullscreen = True
focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
# wmname = "LG3D"
wmname = "LG3D"


widget_defaults = {"font": "FiraCode Nerd Font", "fontsize": 12, "padding": 3}
