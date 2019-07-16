from __future__ import (absolute_import, division, print_function)

# You can import any python module as needed.
import os

# You always need to import ranger.api.commands here to get the Command class:
from ranger.api.commands import Command

fzf_opts = "--ansi \
            --color bg:#1B2B34,fg:#C0C5CE,bg+:#4F5B66,fg+:#D8DEE9,hl:#99C794,hl+:#99C794 \
            --color info:108,prompt:109,spinner:108,pointer:168,marker:#EC5f67"

class fzf_select(Command):
    """
    :fzf_select

    Find a file using fzf.

    With a prefix argument select only directories.

    See: https://github.com/junegunn/fzf
    """
    def execute(self):
        import subprocess
        import os.path
        if self.quantifier:
            # match only directories
            # command="find -L . \( -path '*/\.*' -o -fstype 'dev' -o -fstype 'proc' \) -prune \
            # -o -type d -print 2> /dev/null | sed 1d | cut -b3- | fzf +m"
            command="fd -L . --type d --hidden --color=always | fzf {} +m".format(fzf_opts)

        else:
            # match files and directories
            # command="find -L . \( -path '*/\.*' -o -fstype 'dev' -o -fstype 'proc' \) -prune \
            # -o -print 2> /dev/null | sed 1d | cut -b3- | fzf +m"
            command="fd -L . --hidden --color=always | fzf {} +m".format(fzf_opts)
        fzf = self.fm.execute_command(command, universal_newlines=True, stdout=subprocess.PIPE)
        stdout, stderr = fzf.communicate()
        if fzf.returncode == 0:
            fzf_file = os.path.abspath(stdout.rstrip('\n'))
            if os.path.isdir(fzf_file):
                self.fm.cd(fzf_file)
            else:
                self.fm.select_file(fzf_file)
