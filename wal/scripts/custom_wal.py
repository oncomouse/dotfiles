import sys
from os import system
from pathlib import Path
from shutil import copyfile

import pywal

home = str(Path.home())
new_args = " ".join(sys.argv[1:])
system("wal {}".format(new_args))

# Make a temporary vim dir:
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
