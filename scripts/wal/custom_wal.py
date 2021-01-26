import re
import sys
from os import path
from os import system
from pathlib import Path
from shutil import copyfile
from shutil import which

import pywal

home = str(Path.home())
new_args = " ".join(sys.argv[1:])
system("wal {}".format(new_args))

if re.search("(-R|-i|--(theme|backend) [^-])", new_args) is not None:
    # Make a vim dir to add to &runtimepath:
    Path("{}/.cache/wal/vim/colors/".format(home)).mkdir(parents=True, exist_ok=True)
    Path("{}/.cache/wal/vim/autoload/clap/themes/".format(home)).mkdir(
        parents=True, exist_ok=True
    )
    copyfile(
        "{}/.cache/wal/colors.vim".format(home),
        "{}/.cache/wal/vim/colors/wal.vim".format(home),
    )
    copyfile(
        "{}/.cache/wal/colors-clap.vim".format(home),
        "{}/.cache/wal/vim/autoload/clap/themes/wal.vim".format(home),
    )
    # reload running Neovim instances:
    system('{}/dotfiles/scripts/vim/cmdnvim.sh "source \\$MYVIMRC"'.format(home))
    if which("dunst") is not None:
        system("mkdir -p {}/.config/dunst".format(home))
        copyfile(
            "{}/.cache/wal/dunstrc".format(home),
            "{}/.config/dunst/dunstrc".format(home),
        )
    if which("xrdb") is not None:
        system("xrdb {}/.Xresources".format(home))
    if which("kitty") is not None:
        system("kitty @ set-colors -a -c {}/.cache/wal/colors-kitty.conf".format(home))
    if which("fish") is not None:
        system('fish -c "source {}/.cache/wal/colors-fzf.fish"'.format(home))
    if which("dunst") is not None:
        system("killall dunst; notify-send 'Dunst Reloaded'")
    if which("dwm-msg") is not None:
        system("dwm-msg run_command xrdb > /dev/null")
    if which("bat") is not None:
        if not path.isdir("{}/.config/bat/themes/".format(home)):
            system("mkdir -p {}/.config/bat/themes".format(home))
        if not path.isfile("{}/.config/bat/themes/wal.tmTheme".format(home)):
            system(
                "ln -sf {home}/.cache/wal/colors.tmTheme {home}/.config/bat/themes/wal.tmTheme".format(
                    home=home
                )
            )
        system("bat cache --build")
    if which("pywalfox") is not None:
        system("pywalfox update")
    # Run oomox:
    if which("oomox-cli") is not None:
        system("oomox-cli /opt/oomox/scripted_colors/xresources/xresources-reverse")
    if which("oomox-archdroid-icons-cli"):
        system(
            "oomox-archdroid-icons-cli /opt/oomox/scripted_colors/xresources/xresources-reverse"
        )
