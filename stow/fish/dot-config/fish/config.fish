if status is-interactive
  # Test for keyring:
  if test -n "$DESKTOP_SESSION"
    set -x (gnome-keyring-daemon --start | string split "=")
  end
  # Setup Pywal colors:
  if test -d $HOME/.cache/wal
    source ~/.cache/wal/colors.fish
  end
  # Tell sxhkd to not use fish:
  setuvar SXHKD_SHELL sh
  # NNN Theme
  setuvar NNN_FCOLORS "0603040200050E070D09abc4"
# Configure FZF:
  setuvar FZF_DEFAULT_OPTS "--ansi --bind='ctrl-o:execute(open {})+abort'"
  # Setup FZF themes:
  if test -d "$HOME/.cache/wal" -a (echo $FZF_DEFAULT_OPTS | grep color -c) -eq 0
    echo "Sourcing FZF_COLORS"
    source ~/.cache/wal/colors-fzf.fish
  end
  setuvar FZF_DEFAULT_COMMAND "fd -t f --follow"
  setuvar FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
  setuvar FZF_ALT_C_COMMAND "fd --type d --follow"

  setuvar FZF_CTRL_T_OPTS "--preview-window 'right:60%' --preview 'bat --theme=wal --color=always --style=header,grid --line-range :300 {}'"

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
  setuvar pisces_only_insert_at_eol 1
  #
  # Setup Kitty:
  if command -sq kitty
    kitty + complete setup fish | source
  end

  # Fasd Aliases:
  if command -sq nvim
    function v;f -t -e nvim $argv;end
  else
    function v;f -t -b viminfo -e vim $argv;end
  end

  # Colors:
  set fish_color_cwd cyan
end
