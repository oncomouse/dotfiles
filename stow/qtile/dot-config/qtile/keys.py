# type: ignore
import os

from libqtile.command import lazy
from libqtile.config import Click
from libqtile.config import Drag
from libqtile.config import Key

HOME = os.path.expanduser("~/")

BROWSER = "firefox"
TERM_EMULATOR = "kitty"
MUSIC_PLAYER = "spotify"

mod = "mod1"

keys = [
    # Move around the windows.
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key(
        [mod, "shift"],
        "h",
        lazy.layout.shuffle_left(),
        desc="Move window to the left",
    ),
    Key(
        [mod, "shift"],
        "l",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key(
        [mod, "control"],
        "h",
        lazy.layout.grow_left(),
        desc="Grow window to the left",
    ),
    Key(
        [mod, "control"],
        "l",
        lazy.layout.grow_right(),
        desc="Grow window to the right",
    ),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle Floating
    Key([mod], "f", lazy.window.toggle_floating(), desc="Toggle Floating"),
    # Move Floating Window:
    Key([mod, "shift"], "Up", lazy.window.move_floating(0, -20), desc="Move Up"),
    Key([mod, "shift"], "Down", lazy.window.move_floating(0, 20), desc="Move Down"),
    Key([mod, "shift"], "Left", lazy.window.move_floating(-20, 0), desc="Move Left"),
    Key([mod, "shift"], "Right", lazy.window.move_floating(20, 0), desc="Move Right"),
    # Apps:
    Key([mod, "shift"], "Return", lazy.spawn(TERM_EMULATOR)),
    Key([mod], "p", lazy.spawn("rofi -Show combi -Show-icons")),
]


# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag([mod], "3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "2", lazy.window.bring_to_front()),
]
