import argparse
import errno
import os
import pathlib
import re
from os import path


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
parser.add_argument("--verbose", "-v", action="store_true")
parser.add_argument("--test", action="store_true")
parser.add_argument("stash")
args = parser.parse_args()

stash_dir = path.join(path.realpath(args.source_dir), args.stash)
if not path.isdir(stash_dir):
    raise FileNotFoundError(errno.ENOENT, os.strerror(errno.ENOENT), stash_dir)
for file in pathlib.Path(stash_dir).glob("**/*"):
    file_name = str(file)
    output_file = path.join(
        args.output_dir, guess_filename(file_name.replace(stash_dir + "/", ""))
    )
    if not path.isdir(path.dirname(output_file)):
        if args.test or args.verbose:
            print("Making path at: {}".format(path.dirname(output_file)))
        if not args.test:
            pathlib.Path(path.dirname(output_file)).mkdir(parents=True, exist_ok=True)
    if path.isfile(output_file):
        if path.islink(output_file):
            if path.realpath(output_file) != file_name:
                if args.test or args.verbose:
                    print("Removing link at: {}".format(output_file))
                if not args.test:
                    os.unlink(output_file)
            else:
                if args.test or args.verbose:
                    print("Skipping: {}".format(file_name))
                continue
        else:
            if args.test or args.verbose:
                print("Renaming: {} to {}".format(output_file, output_file + ".bak"))
            if not args.test:
                os.rename(output_file, output_file + ".bak")
    if args.test or args.verbose:
        print("Linking: {} to {}".format(file_name, output_file))
    if not args.test:
        os.link(file_name, output_file)
