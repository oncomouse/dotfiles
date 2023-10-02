import re
import sys
from os import environ
from os import path
from os import system
from os import WEXITSTATUS
from pathlib import Path
from shutil import copyfile
from shutil import which

from colorama import Fore
from colorama import init
from colorama import Style

# import pywal

init(autoreset=True)

home = str(Path.home())
new_args = " ".join(sys.argv[1:])
system("wal {}".format(new_args))


def message(msg, output="info"):
    color = Fore.GREEN
    symbol = "I"
    if output == "warning":
        color = Fore.YELLOW
        symbol = "W"
    print(
        "["
        + color
        + Style.BRIGHT
        + symbol
        + Style.RESET_ALL
        + "] "
        + Fore.RED
        + Style.BRIGHT
        + "update"
        + Style.RESET_ALL
        + ": "
        + msg,
        end="\n",
    )


def pgrep(process):
    return (
        WEXITSTATUS(
            system("pgrep -c {} > /dev/null 2> /dev/null".format(process))
        )
        == 0
    )


if re.search("(-R|-i|--(theme|backend) [^-])", new_args) is not None:
    message("New background")
    system("convert ~/.cache/wal/background.svg ~/.cache/wal/background.png")
    system("tmux run '~/.tmux/themes/wal.tmux'")
    if which("hsetroot") is not None:
        system("hsetroot -tile ~/.cache/wal/background.png")

    if which("xrdb") is not None:
        message("Reloading xrdb")
        system("xrdb -I{} ~/.Xresources".format(home))

    # Reload polybar:
    if pgrep("polybar"):
        system("polybar-msg cmd restart")

    # Reload various windowmanagers:
    if pgrep("awesome"):
        message("Reloading awesome")
        system('echo "awesome:restart()" | awesome-client')
    if pgrep("sdorfehs"):
        system("/bin/bash -c sf-colors.sh")
    if pgrep("dwm"):
        system("xdotool key super+F5")

    if pgrep("st"):
        system("pidof st | xargs kill -s USR1")

    if re.match(r"kitty", environ["TERM"]) is not None:
        message("Reloading kitty")
        system(
            "kitty @ set-colors -a -c {}/.cache/wal/colors-kitty.conf".format(
                home
            )
        )
    if which("cava") is not None:
        message("Configure CAVA colors")
        system("mkdir -p {}/.config/cava".format(home))
        copyfile(
            "{}/.cache/wal/colors-cava".format(home),
            "{}/.config/cava/config".format(home),
        )
    if which("bat") is not None:
        message("Configuring bat")
        if not path.isdir("{}/.config/bat/themes/".format(home)):
            system("mkdir -p {}/.config/bat/themes".format(home))
        if not path.isfile("{}/.config/bat/themes/wal.tmTheme".format(home)):
            system(
                "ln -sf {home}/.cache/wal/colors.tmTheme {home}/.config/bat/themes/wal.tmTheme".format(
                    home=home
                )
            )
        system("bat cache --build > /dev/null")
    if which("xfce4-terminal") is not None:
        system("mkdir -p ~/.local/share/xfce4/terminal/colorschemes")
        system(
            "ln -sf ~/.cache/wal/pywal.theme ~/.local/share/xfce4/terminal/colorschemes"
        )
