import re
import sys
from os import environ
from os import path
from os import system
from pathlib import Path
from shutil import copyfile
from shutil import which

from colorama import Back
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


if re.search("(-R|-i|--(theme|backend) [^-])", new_args) is not None:
    # Make a vim dir to add to &runtimepath:
    message("Configuring Vim")
    Path("{}/.cache/wal/vim/colors/".format(home)).mkdir(parents=True, exist_ok=True)
    # Path("{}/.cache/wal/vim/autoload/clap/themes/".format(home)).mkdir(
    #     parents=True, exist_ok=True
    # )
    copyfile(
        "{}/.cache/wal/colors.vim".format(home),
        "{}/.cache/wal/vim/colors/wal.vim".format(home),
    )
    # copyfile(
    #     "{}/.cache/wal/colors-clap.vim".format(home),
    #     "{}/.cache/wal/vim/autoload/clap/themes/wal.vim".format(home),
    # )
    # reload running Neovim instances:
    # message("Restarting Neovim")
    # system('{}/dotfiles/scripts/vim/cmdnvim.sh "source \\$MYVIMRC"'.format(home))
    if which("dunst") is not None:
        message("Configuring Dunst")
        system("mkdir -p {}/.config/dunst > /dev/null".format(home))
        copyfile(
            "{}/.cache/wal/dunstrc".format(home),
            "{}/.config/dunst/dunstrc".format(home),
        )
        message("Restarting Dunst")
        system("killall dunst > /dev/null 2>&1 && notify-send 'Dunst Reloaded'")
    if which("xrdb") is not None:
        message("Reloading xrdb")
        system("xrdb {}/.Xresources".format(home))
    if which("aweseome-client") is not None:
        system('echo "awesome:restart()" | awesome-client')
    if re.match(r"kitty", environ["TERM"]) is not None:
        message("Reloading kitty")
        system("kitty @ set-colors -a -c {}/.cache/wal/colors-kitty.conf".format(home))
    if which("fish") is not None:
        message("Configuring fish FZF colors")
        system('fish -c "source {}/.cache/wal/colors-fzf.fish"'.format(home))
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
    if which("pywalfox") is not None:
        message("Updaing pywalfox")
        system("pywalfox update")
    # Run oomox:
    if which("oomox-cli") is not None:
        message("Running oomox-cli")
        system(
            "oomox-cli /opt/oomox/scripted_colors/xresources/xresources-reverse > /dev/null"
        )
    if which("oomox-archdroid-icons-cli"):
        message("Running oomox-archdroid-icons-cli")
        system(
            "oomox-archdroid-icons-cli /opt/oomox/scripted_colors/xresources/xresources-reverse > /dev/null"
        )
