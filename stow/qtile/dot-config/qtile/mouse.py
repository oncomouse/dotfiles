from libqtile.command import lazy
from libqtile.config import EzClick as Click
from libqtile.config import EzDrag as Drag

# Drag floating layouts.
mouse = [
    Drag(
        "M-Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag("M-Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click("M-Button2", lazy.window.bring_to_front()),
]
