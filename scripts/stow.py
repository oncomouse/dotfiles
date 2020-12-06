"""Reimplementation of *most* of GNU Stow in Python (and working).

This reimplements a bunch of the features from GNU Stow, but does not implement
--adopt or any of the regex switches. This exists because GNU Stow has had a
bug in the implementation of --dotfiles for over a year, a patch for that for
around 8 months, and it continues to not work. And guess what feature I needed
to get my dotfiles working?

stow.py -h has documentation of usage.
"""
import argparse
import errno
import os
import pathlib
from collections import deque
from os import path


def guess_filename(file):
    """Figure out the file's name when it has been deployed."""
    global args
    if args.dotfiles:
        return file.replace("dot-", ".")
    return file


def output_file_exists(input_file, output_file):
    """Makes sense of an existing file and plans stow's response."""
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
    """Parse the indicated repository and stow the necessary files.

    The logic of this function is pretty complicated, but it seems to work best
    with the intended spirit of GNU stow. We use a queue structure to parse the
    repository, making sure to build directories or symlink things as they
    become needed. This appears to be the best way to implement folding or to
    avoid folding when required.
    """
    global stash_dir, args
    stow_files = deque([str(file) for file in pathlib.Path(stash_dir).glob("*")])
    while len(stow_files) > 0:
        input_file = stow_files.pop()
        output_file = path.join(
            args.output_dir, guess_filename(input_file.replace(stash_dir + "/", ""))
        )
        if path.isdir(input_file):
            if path.isdir(output_file) or args.no_folding:
                for file in pathlib.Path(input_file).glob("*"):
                    stow_files.append(str(file))
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
    """Handle deleting stowed files."""
    global stash_dir, args
    # We track directories so we can remove any we empty:
    directories = []
    for file in pathlib.Path(stash_dir).glob("**/*"):
        input_file = str(file)
        output_file = path.join(
            args.output_dir, guess_filename(input_file.replace(stash_dir + "/", ""))
        )
        if path.isdir(output_file):
            directories.append(output_file)
        if path.islink(output_file):
            real_path = str(pathlib.Path(output_file).resolve())
            if real_path == input_file:
                if args.no or args.verbose:
                    print("Unlinking: {}".format(output_file))
                if not args.no:
                    os.unlink(output_file)
    # Remove any empty directories we might have created when stowing:
    for directory in directories:
        if not path.isdir(directory) and not path.islink(directory):
            continue
        if len(os.listdir(directory)) == 0:
            if args.no or args.verbose:
                print("Unlinking: {}".format(directory))
            if not args.no:
                os.rmdir(directory)
        else:
            for file in pathlib.Path(directory).glob("*"):
                if path.islink(file) and not os.path.exists(os.readlink(str(file))):
                    if not args.no:
                        os.unlink(str(file))


# Arguments:
parser = argparse.ArgumentParser(
    description="Stow manages dotfiles (let's be honest, that's what we're using it for)."
)
parser.add_argument(
    "--to",
    "-t",
    dest="output_dir",
    default="..",
    help='The directory to output in (default is "..").',
)
parser.add_argument(
    "--dir",
    "-d",
    dest="source_dir",
    default=".",
    help='The directory to source stow repositories from (default is ".").',
)
parser.add_argument(
    "--delete",
    "-D",
    action="store_true",
    help="Delete a previously stowed repository by removing the deployed symlinks.",
)
parser.add_argument(
    "--restow",
    "-R",
    action="store_true",
    help="Delete and then stow a previously stowed repository. This is useful for cleaning dead symlinks.",
)
parser.add_argument(
    "--verbose",
    "-v",
    action="store_true",
    help="Print a detailed account of what is happening.",
)
parser.add_argument(
    "--no",
    "-n",
    action="store_true",
    help="Only test the action being proposed. Is not necessary to use --verbose with this argument.",
)
parser.add_argument(
    "--dotfiles",
    action="store_true",
    help='If true, any usage of "dot-" in the stow repository will be translated to "." when deployed. Useful with git repositories.',
)
parser.add_argument(
    "--no-folding",
    action="store_true",
    help="If true, each file will be symlinked individually from the stow repository to the deployed location. If false, where possible, whole directories will be symlinked instead.",
)
parser.add_argument(
    "--overwrite",
    action="store_true",
    help="If true, stow will remove any files in its way as it tries to symlink the repository.",
)
parser.add_argument(
    "stash",
    help="The name of the stow repository (directory located inside of --dir) to stow.",
)
args = parser.parse_args()

stash_dir = path.join(path.realpath(args.source_dir), args.stash)
if not path.isdir(stash_dir):
    raise FileNotFoundError(errno.ENOENT, os.strerror(errno.ENOENT), stash_dir)
if args.delete or args.restow:
    delete()
if not args.delete:
    ## Find a way to delete dead links
    stow()
