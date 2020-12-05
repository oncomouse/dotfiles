import argparse
import errno
import pathlib
import re
from os import path
from os import strerror


def guess_filename(file):
    if re.match(r"^\.", file) is not None:
        return file
    if file == "config":
        return ".config"
    if re.match(r"^[A-Z]", file) is not None:
        return file
    return ".{}".format(file)


# Arguments:
parser = argparse.ArgumentParser(description="Manage my dotfiles.")
parser.add_argument("--to", "-t", dest="output_dir", default="..")
parser.add_argument("--dir", "-d", dest="source_dir", default=".")
parser.add_argument("stash")
args = parser.parse_args()

stash_dir = path.join(path.realpath(args.source_dir), args.stash)
if not path.isdir(stash_dir):
    raise FileNotFoundError(errno.ENOENT, strerror(errno.ENOENT), stash_dir)
source_files = []
for file in pathlib.Path(stash_dir).glob("*"):
    print(guess_filename(path.basename(str(file))))
    source_files.append(str(file))
    ## If file is a directory, generate a list of files, mkdir -p the root, and symbolically link
    ## each file
