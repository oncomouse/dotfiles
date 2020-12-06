import argparse
import errno
import os
import pathlib
import re
from collections import deque
from os import path
from shutil import copy2


def guess_filename(file):
    global args
    if args.dotfiles:
        return file.replace("dot-", ".")
    return file


def output_file_exists(input_file, output_file):
    # Path is a link:
    if path.islink(output_file):
        # Path is a link to a stowed file:
        if path.realpath(output_file) == input_file:
            raise ResourceWarning("File is one of ours")
        # Path is a link to a file not in our package:
        raise FileExistsError(
            errno.EEXIST,
            "File is a link but not to stow package: {}".format(
                path.realpath(output_file)
            ),
        )
    raise FileExistsError(
        errno.EEXIST,
        "File already exists: {}".format(path.realpath(output_file)),
    )


def stow():
    global stash_dir, args
    stow_files = deque([str(file) for file in pathlib.Path(stash_dir).glob("*")])
    while len(stow_files) > 0:
        input_file = stow_files.pop()
        output_file = path.join(
            args.output_dir, guess_filename(input_file.replace(stash_dir + "/", ""))
        )
        if path.isdir(input_file):
            if path.isdir(output_file) or args.no_folding:
                [
                    stow_files.append(str(file))
                    for file in pathlib.Path(input_file).glob("*")
                ]
            else:
                if args.no or args.verbose:
                    print("Linking directory: {} -> {}".format(input_file, output_file))
                if not args.no:
                    os.symlink(input_file, output_file)
        else:
            ## Make the output directory, if it does not exist:
            if not path.isdir(path.dirname(output_file)):
                if args.no or args.verbose:
                    print("Making path at: {}".format(path.dirname(output_file)))
                if not args.no:
                    pathlib.Path(path.dirname(output_file)).mkdir(
                        parents=True, exist_ok=True
                    )
            # Path exists:
            if path.isfile(output_file):
                if args.overwrite:
                    if args.no or args.verbose:
                        print("Removing existing file at: {}".format(output_file))
                    if not args.no:
                        os.unlink(output_file)
                else:
                    try:
                        output_file_exists(input_file, output_file)
                    except FileExistsError:
                        print("File already exists (skipping): {}".format(output_file))
                        continue
                    except ResourceWarning:
                        if args.no or args.verbose:
                            print("Skipping: {}".format(input_file))
                        continue
            if args.no or args.verbose:
                print("Linking: {} to {}".format(input_file, output_file))
            if not args.no:
                os.symlink(input_file, output_file)


def delete():
    global stash_dir, args
    directories = []
    for file in pathlib.Path(stash_dir).glob("**/*"):
        file_name = str(file)
        output_file = path.join(
            args.output_dir, guess_filename(file_name.replace(stash_dir + "/", ""))
        )
        if path.isdir(output_file):
            directories.append(output_file)
        if path.islink(output_file):
            real_path = str(pathlib.Path(output_file).resolve())
            if real_path == file_name:
                if args.no or args.verbose:
                    print("Unlinking: {}".format(output_file))
                if not args.no:
                    os.unlink(output_file)
    # Remove any empty directories we might have created when stowing:
    for directory in directories:
        if len(os.listdir(directory)) == 0:
            if args.no or args.verbose:
                print("Unlinking: {}".format(directory))
            if not args.no:
                os.rmdir(directory)


# Arguments:
parser = argparse.ArgumentParser(description="Manage my dotfiles.")
parser.add_argument("--to", "-t", dest="output_dir", default="..")
parser.add_argument("--dir", "-d", dest="source_dir", default=".")
parser.add_argument("--delete", "-D", action="store_true")
parser.add_argument("--restow", "-R", action="store_true")
parser.add_argument("--verbose", "-v", action="store_true")
parser.add_argument("--no", "-n", action="store_true")
parser.add_argument("--dotfiles", action="store_true")
parser.add_argument("--no-folding", action="store_true")
parser.add_argument("--overwrite", action="store_true")
parser.add_argument("stash")
args = parser.parse_args()

stash_dir = path.join(path.realpath(args.source_dir), args.stash)
if not path.isdir(stash_dir):
    raise FileNotFoundError(errno.ENOENT, os.strerror(errno.ENOENT), stash_dir)
if args.delete or args.restow:
    delete()
if not args.delete:
    ## Find a way to delete dead links
    stow()
