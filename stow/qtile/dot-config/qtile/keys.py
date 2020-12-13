from groups import groups
from libqtile.command import lazy
from libqtile.config import EzClick as Click
from libqtile.config import EzDrag as Drag
from libqtile.config import Key

BROWSER = "firefox"
TERM_EMULATOR = "kitty"
MUSIC_PLAYER = "spotify"

keys = [
    # Move around the windows.
    Key(["mod1"], "h", lazy.layout.left(), desc="Move focus to left"),
    Key(["mod1"], "l", lazy.layout.right(), desc="Move focus to right"),
    Key(["mod1"], "j", lazy.layout.down(), desc="Move focus down"),
    Key(["mod1"], "k", lazy.layout.up(), desc="Move focus up"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key(
        ["mod1", "shift"],
        "h",
        lazy.layout.shuffle_left(),
        desc="Move window to the left",
    ),
    Key(
        ["mod1", "shift"],
        "l",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key(["mod1", "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key(["mod1", "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key(
        ["mod1", "control"],
        "h",
        lazy.layout.grow_left(),
        desc="Grow window to the left",
    ),
    Key(
        ["mod1", "control"],
        "l",
        lazy.layout.grow_right(),
        desc="Grow window to the right",
    ),
    Key(["mod1", "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key(["mod1", "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key(["mod1"], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle Floating
    Key(["mod1"], "f", lazy.window.toggle_floating(), desc="Toggle Floating"),
    # Move Floating Window:
    Key(["mod1", "shift"], "Up", lazy.window.move_floating(0, -20), desc="Move Up"),
    Key(["mod1", "shift"], "Down", lazy.window.move_floating(0, 20), desc="Move Down"),
    Key(["mod1", "shift"], "Left", lazy.window.move_floating(-20, 0), desc="Move Left"),
    Key(
        ["mod1", "shift"], "Right", lazy.window.move_floating(20, 0), desc="Move Right"
    ),
    # Apps:
    Key(["mod1", "shift"], "Return", lazy.spawn(TERM_EMULATOR)),
    Key(["mod1"], "p", lazy.spawn("rofi -Show combi -Show-icons")),
]

for i in groups:
    keys.append(Key(["mod1"], "{}".format(i.name), lazy.group[i.name].toscreen()))
    keys.append(
        Key(
            ["mod1", "shift"],
            "{}".format(i.name),
            lazy.window.togroup(i.name, switch_group=True),
        )
    )

# Drag floating layouts.
mouse = [
    Drag(
        "M-1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag("M-3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click("M-2", lazy.window.bring_to_front()),
]
