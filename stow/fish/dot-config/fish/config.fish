# Dotfiles utility functions:
source $HOME/dotfiles/conf/fish/dotfiles.fish

# The rest of this configuration file only needs to load if shell is interactive:
if status is-interactive
  # Test for keyring:
  if test -n "$DESKTOP_SESSION"
    set -x (gnome-keyring-daemon --start | string split "=")
  end
  # Setup Pywal colors:
  if test -d $HOME/.cache/wal
    source ~/.cache/wal/colors.fish
  end
  if not set -q -U NNN_FCOLORS
    echo "Setting NNN_FCOLORS"
    set -Ux NNN_FCOLORS "0603040200050E070D09abc4"
  end
# Configure FZF:
  if not set -q FZF_DEFAULT_OPTS
    set -Ux FZF_DEFAULT_OPTS "--ansi --bind='ctrl-o:execute(open {})+abort'"
  end
  # Setup FZF themes:
  if test -d "$HOME/.cache/wal" -a (echo $FZF_DEFAULT_OPTS | grep color -c) -eq 0
    echo "Sourcing FZF_COLORS"
    source ~/.cache/wal/colors-fzf.fish
  end
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
    if test -n "$result"
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

  # Load the asdf wrapper function
  source $ASDF_DIR/lib/asdf.fish

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
