1. `/usr/local/opt/ncurses/bin/infocmp -x tmux-256color > ~/tmux-256color.src`
2. `nvim ~/tmux-256color.src`
        * Change `pairs#0x10000` or `pairs#65536` TO `pairs#32767`
3. `sudo /usr/bin/tic -x -o /usr/share/terminfo ~/tmux-256color.src`
