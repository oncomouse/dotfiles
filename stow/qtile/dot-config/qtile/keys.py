from groups import groups
from libqtile.command import lazy
from libqtile.config import EzKey as Key

BROWSER = "firefox"
TERM_EMULATOR = "kitty"
MUSIC_PLAYER = "spotify"

keys = [
    # Move around the windows.
    Key("A-h", lazy.layout.left(), desc="Move focus to left"),
    Key("A-l", lazy.layout.right(), desc="Move focus to right"),
    Key("A-j", lazy.layout.down(), desc="Move focus down"),
    Key("A-k", lazy.layout.up(), desc="Move focus up"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key("A-s-h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key("A-s-l", lazy.layout.shuffle_right(), desc="Move window to the right"),
    Key("A-s-j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key("A-s-k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key("A-c-h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key("A-c-l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key("A-c-j", lazy.layout.grow_down(), desc="Grow window down"),
    Key("A-c-k", lazy.layout.grow_up(), desc="Grow window up"),
    Key("A-n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle Floating
    Key("A-f", lazy.window.toggle_floating(), desc="Toggle Floating"),
    # Move Floating Window:
    Key("A-s-Up", lazy.window.move_floating(0, -20), desc="Move Up"),
    Key("A-s-Down", lazy.window.move_floating(0, 20), desc="Move Down"),
    Key("A-s-Left", lazy.window.move_floating(-20, 0), desc="Move Left"),
    Key("A-s-Right", lazy.window.move_floating(20, 0), desc="Move Right"),
    # Apps:
    Key("A-s-Return", lazy.spawn(TERM_EMULATOR)),
    Key("A-p", lazy.spawn("rofi -show combi -show-icons")),
]

for i in groups:
    keys.append(Key("A-{}".format(i.name), lazy.group[i.name].toscreen()))
    keys.append(
        Key("A-s-{}".format(i.name), lazy.window.togroup(i.name, switch_group=True))
    )
