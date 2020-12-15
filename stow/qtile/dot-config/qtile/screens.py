# type: ignore
from colors import colors
from groups import group_names
from keys import HOME
from libqtile import bar
from libqtile import widget
from libqtile.config import Screen

# from keys import launcher

# from playerfunc import currently_playing

TELA_ICONS = "/usr/share/icons/Numix/"

left_sep = ""
sep = ""
right_sep = ""

separator_defaults = dict(
    font="FiraCode Nerd Font Mono",
    fontsize=21,
    padding=0,
    margin=0,
)


widget_defaults = dict(
    font="Fira Code Bold",
    fontsize=15,
    padding=0,
    margin=0,
    foreground=colors["color15"],
)

extension_defaults = widget_defaults.copy()

widgets = [
    widget.TextBox(
        **separator_defaults,
        text=left_sep,
        background=colors["color8"],
        foreground=colors["color4"],
    ),
    widget.GroupBox(
        background=colors["color4"],
        active=colors["color4"],
        block_highlight_text_color=colors["color8"],
        highlight_color=colors["color9"],
        highlight_method="line",
        borderwidth=0,
        rounded=False,
        margin_y=3,
        margin_x=2,
        padding_x=8,
        fmt="<b>{}</b>",
        urgent_border=colors["color13"],
        visible_groups=group_names,  # Fix bug of extra groups appearing from default config
    ),
    widget.TextBox(
        **separator_defaults,
        text=right_sep,
        background=colors["color8"],
        foreground=colors["color4"],
    ),
    # widget.TaskList(
    #     background=colors["tasklist_bg"],
    #     border=colors["tasklist_bg"],
    #     borderwidth=2,
    #     highlight_method="block",
    #     # icon_size=20,
    #     margin_y=3,
    #     margin_x=3,
    #     padding_x=3,
    #     padding_y=3,
    #     spacing=0,
    #     # max_title_width=24,
    #     markup_floating="",
    #     markup_focused="",
    #     markup_maximized="",
    #     markup_minimized="",
    #     markup_normal="",
    # ),
    # # Center
    widget.WindowName(fmt="{}"),
    # widget.TextBox(
    #     **separator_defaults,
    #     text=left_sep,
    #     background=colors["bg"],
    #     foreground=colors["player_bg"],
    # ),
    # # widget.GenPollText(
    # #     func=currently_playing,
    # #     mouse_callbacks={
    # #         "Button1": lambda qtile: qtile.cmd_spawn("playerctl play-pause"),
    # #         "Button4": lambda qtile: qtile.cmd_spawn("playerctl previous"),
    # #         "Button5": lambda qtile: qtile.cmd_spawn("playerctl next"),
    # #     },
    # #     update_interval=2,
    # #     background=colors["player_bg"],
    # #     padding=5,
    # # ),
    # widget.TextBox(
    #     **separator_defaults,
    #     text=sep,
    #     foreground=colors["sep"],
    #     background=colors["player_bg"],
    # ),
    # widget.BatteryIcon(
    #     theme_path=TELA_ICONS + "24/panel/",
    #     background=colors["battery_bg"],
    #     mouse_callbacks={
    #         "Button4": lambda qtile: qtile.cmd_spawn("xbacklight -inc 10"),
    #         "Button5": lambda qtile: qtile.cmd_spawn("xbacklight -dec 10"),
    #     },
    #     update_interval=1,
    # ),
    # # widget.Backlight(
    # #     backlight_name="intel_backlight",
    # #     brightness_file="brightness",
    # #     max_brightness_file="max_brightness",
    # #     # fmt="{0} ",
    # #     format="{percent:2.0%}",
    # #     background=colors["battery_bg"],
    # #     step=10,
    # #     # change_command='xbacklight -set {0}',
    # #     change_command="xbacklight -set {0}",
    # #     update_interval=0.2,
    # #     # mouse_callbacks={
    # #     # 'Button4': lambda qtile: qtile.cmd_spawn('xbacklight -inc 10'),
    # #     # 'Button5': lambda qtile: qtile.cmd_spawn('xbacklight -dec 10'),
    # #     # }
    # # ),
    # widget.TextBox(
    #     **separator_defaults,
    #     text=sep,
    #     foreground=colors["sep"],
    #     background=colors["battery_bg"],
    # ),
    # widget.Volume(
    #     step=5,
    #     padding=0,
    #     margin=0,
    #     theme_path=TELA_ICONS + "24/panel/",
    #     volume_app="pavucontrol",
    #     background=colors["volume_bg"],
    # ),
    # widget.Volume(
    #     step=5,
    #     padding=0,
    #     margin=0,
    #     volume_app="pavucontrol",
    #     fmt=" {0} ",
    #     background=colors["volume_bg"],
    # ),
    widget.TextBox(
        **separator_defaults,
        text=left_sep,
        foreground=colors["color4"],
        background=colors["color8"],
    ),
    widget.Clock(
        background=colors["color4"],
        foreground=colors["color8"],
        format="%d %B | %H:%M",
        fmt="<span font_family='Fira Code Nerd Font' size='larger'> </span> {}",
        padding=4,
    ),
    widget.TextBox(
        **separator_defaults,
        text=right_sep,
        background=colors["color8"],
        foreground=colors["color4"],
    ),
]

screens = [Screen(top=bar.Bar(widgets, 28, background=colors["color8"]))]
