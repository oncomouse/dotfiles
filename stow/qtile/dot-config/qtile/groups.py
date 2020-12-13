from keys import keys
from keys import mod
from libqtile.config import Group
from libqtile.config import Key
from libqtile.lazy import lazy

groups = []

group_names = ["1", "2", "3", "4", "5"]

group_labels = ["WEB", "DEV", "TERM", "SYS", "MUS"]

for i in range(len(group_names)):
    groups.append(
        Group(
            name=group_names[i],
            label=group_labels[i],
        )
    )

for i in groups:
    keys.extend(
        [
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            Key(
                [mod, "control"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
        ]
    )
