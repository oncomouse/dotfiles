if status --is-login
  # Local paths:
  function add_to_user_paths -a dir
    # Delete $dir if it is present but does not exist:
    if set -l index (contains -i $dir $fish_user_paths)
      if not test -d $dir
        set -e -U fish_user_paths[$index]
      end
    # Add $dir if it is not present but does exist:
    else
      if test -d $dir
        echo "Adding" $dir "to fish_user_paths"
        set -Ux fish_user_paths $fish_user_paths $dir
      end
    end
  end
  # Custom paths:
  add_to_user_paths ~/bin
  add_to_user_paths ~/.local/bin
  # Set up Cargo:
  add_to_user_paths ~/.cargo/bin
  # Set up FZF (if local):
  add_to_user_paths ~/.fzf/bin
  # Set up Poetry:
  add_to_user_paths ~/.poetry/bin
  # Set up ASDF:
  add_to_user_paths ~/.asdf/shims
  add_to_user_paths /usr/local/opt/asdf/bin
  add_to_user_paths ~/.asdf/bin
  # Sbin:
  add_to_user_paths /usr/local/sbin
  add_to_user_paths ~/.ghcup/bin
  # NPM Local:
  add_to_user_paths ~/.npm-packages/bin

  # NPM Local manpath:
  set -q MANPATH || set MANPATH ''
  set -gx MANPATH $MANPATH ~/.npm-packages/share/man
end

# Universal ignore for ag
function ag; /usr/bin/env ag --path-to-ignore ~/.ignore --hidden $argv; end
# Other Command Aliases:
function cat;bat --paging=never --theme=wal $argv;end
function icat;kitty +kitten icat $argv; end
function top;htop $argv;end
function ls;exa --group-directories-first $argv;end
function wal;python3 ~/dotfiles/scripts/wal/custom_wal.py $argv;end
# Vim is Neovim in server mode:
function vim
  if command -sq nvim
    eval (which nvim) -u ~/.vimrc $argv
  else
    eval (which vim) $argv
  end
end
# Vi is Neovim or Vim in barebones mode:
function vi
  if command -sq nvim
    eval (which nvim) -u ~/dotfiles/configurations/vim/vimrc-minimal $argv
  else if command -sq vim
    eval (which vim) -u ~/dotfiles/configurations/vim/vimrc-minimal $argv
  else
    eval (which vi) $argv
  end
end
function standard;~/.npm-packages/bin/semistandard $argv | ~/.npm-packages/bin/snazzy;end
function janet-repl;/usr/local/bin/janet -e "(import spork/netrepl) (netrepl/server)";end

# Dotfiles utility functions:
source /Users/apilsch/dotfiles/configurations/fish/dotfiles.fish

# The rest of this configuration file only needs to load if shell is interactive:
if status is-interactive
  # Test for keyring:
  if test -n "$DESKTOP_SESSION"
    set -x (gnome-keyring-daemon --start | string split "=")
  end
  # Setup Pywal colors:
  source ~/.cache/wal/colors.fish
  if not set -q FZF_DEFAULT_OPTS
    set -Ux FZF_DEFAULT_OPTS "--ansi --bind='ctrl-o:execute(open {})+abort'"
  end
  # Setup FZF themes:
  if not set -q -U FZF_COLORS
    source ~/.cache/wal/colors-fzf.fish
  end
  if not set -q -U NNN_FCOLORS
    echo "Setting NNN_FCOLORS"
    set -Ux NNN_FCOLORS "0603040200050E070D09abc4"
  end
# Configure FZF:
  if not set -q -U FZF_DEFAULT_COMMAND
    echo "Setting FZF"
    set -Ux FZF_DEFAULT_COMMAND "fd -t f --hidden --follow"
    set -Ux FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
    set -Ux FZF_ALT_C_COMMAND "fd --type d --hidden --follow"

    set -Ux FZF_CTRL_T_OPTS "--preview-window 'right:60%' --preview 'bat --theme=wal --color=always --style=header,grid --line-range :300 {}'"
  end

  # Open directories in Finder w/ alt+o
  function choose_dir_with_fzf
    eval "fd --type d . $HOME | fzf +m $FZF_DEFAULT_OPTS $FZF_ALT_C_OPTS" | read -l result
    if [ -n "$result" ]
      open $result
    end
  end
  bind \eo choose_dir_with_fzf

  # Configure ASDF:
  # if not contains $HOME/.asdf/shims $fish_user_paths
  #   and test (uname) = "Darwin"
  #   echo "Loading ASDF"
  #   set -Ux ASDF_DIR (brew --prefix asdf)
  #   set -l asdf_data_dir (
  #     if test -n "$ASDF_DATA_DIR"; echo $ASDF_DATA_DIR;
  #     else; echo $HOME/.asdf; end)

  #   # Add asdf to PATH
  #   set -l asdf_bin_dirs $ASDF_DIR/bin $ASDF_DIR/shims $asdf_data_dir/shims
  #   for x in $asdf_bin_dirs
  #     if test -d $x
  #       and not contains $x $fish_user_paths
  #       add_to_user_paths $x
  #     end
  #   end
  # end

  function nvim5;$HOME/.local/bin/nvim-osx64/bin/nvim $argv;end

  # Load the asdf wrapper function
  source $ASDF_DIR/lib/asdf.fish

  # Don't reload default ranger settings:
  if not set -q RANGER_LOAD_DEFAULT_RC
    set -Ux RANGER_LOAD_DEFAULT_RC FALSE
  end

  # Configure Pisces (fish pairing):
  if not set -q pisces_only_insert_at_eol
    set -Ux pisces_only_insert_at_eol 1
  end
  #
  # Setup Kitty:
  if command -sq kitty
    kitty + complete setup fish | source
  end
  # SSH to Dreamhost:
  function pilsch.com; ssh eschaton@birkenfeld.dreamhost.com; end

  # Fasd Aliases:
  if command -sq nvim
    function v;f -t -e nvim $argv;end
  else
    function v;f -t -b viminfo -e vim $argv;end
  end

  # Colors:
  set fish_color_cwd cyan

  # Source: https://github.com/SidOfc/dotfiles/blob/master/config.fish
  function kp --description "Kill processes"
    set -l __kp__pid ''
    set __kp__pid (ps -ef | sed 1d | eval "fzf $FZF_DEFAULT_OPTS -m --header='[kill:process]'" | awk '{print $2}')

    if test "x$__kp__pid" != "x"
      if test "x$argv[1]" != "x"
        echo $__kp__pid | xargs kill $argv[1]
      else
        echo $__kp__pid | xargs kill
      end
      exec kp
    end
  end
end
