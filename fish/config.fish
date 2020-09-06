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
      set -Ux fish_user_paths $fish_user_paths $dir
    end
  end
end
# Custom paths:
add_to_user_paths ~/bin
add_to_user_paths ~/.local/bin
add_to_user_paths /usr/local/sbin
# Set up Cargo:
add_to_user_paths ~/.cargo/bin
# Set up FZF:
add_to_user_paths ~/.fzf/bin

# Universal ignore for ag
function ag; /usr/bin/env ag --path-to-ignore ~/.ignore --hidden $argv; end
# Other Command Aliases:
function cat;bat --paging=never $argv;end
function top;htop $argv;end
function ls;exa --group-directories-first $argv;end
# Vim is Neovim in server mode:
function vim
  if command -sq nvim
    eval (which nvim) -u ~/dotfiles/vim/vimrc-server $argv
  else
    eval (which vim) $argv
  end
end
# Vi is Neovim or Vim in barebones mode:
function vi
  if command -sq nvim
    eval (which nvim) -u ~/dotfiles/vim/vimrc-minimal $argv
  else if command -sq vim
    eval (which vim) -u ~/dotfiles/vim/vimrc-minimal $argv
  else
    eval (which vi) $argv
  end
end
function standard;/usr/local/bin/semistandard $argv | /usr/local/bin/snazzy;end

# Vim related aliases:
if not set -q -U EDITOR 
  echo "Setting EDITOR"
  if command -sq nvim
    set -Ux EDITOR (which nvim)
  else
    set -Ux EDITOR (which vim)
  end
end

# The rest of this configuration file only needs to load if shell is interactive:
if status is-interactive
  # Configure FZF:
  if not set -q -U FZF_DEFAULT_COMMAND
    echo "Setting FZF"
    set -Ux FZF_DEFAULT_COMMAND "fd -t f --hidden --follow"
    set -Ux FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
    set -Ux FZF_ALT_C_COMMAND "fd --type d --hidden --follow"
    set -l FZF_DEFAULT_OPTS "--ansi --bind='ctrl-o:execute(open {})+abort'"\
    " --bind='ctrl-e:execute(nvim {})+abort'"

    # Base 16 but our theme:
    set -l color00 '#232323'
    set -l color01 '#3a3a3a'
    set -l color02 '#515152'
    set -l color03 '#686869'
    set -l color04 '#807f80'
    set -l color05 '#8b8b8c'
    set -l color06 '#979698'
    set -l color07 '#aeadaf'
    set -l color08 '#d58888'
    set -l color09 '#d2813d'
    set -l color0A '#b1942b'
    set -l color0B '#96a42d'
    set -l color0C '#7aa880'
    set -l color0D '#8e9cc0'
    set -l color0E '#b58d88'
    set -l color0F '#d0913d'

    set -l FZF_NON_COLOR_OPTS

    for arg in (echo $FZF_DEFAULT_OPTS | tr " " "\n")
        if not string match -q -- "--color*" $arg
            set -a FZF_NON_COLOR_OPTS $arg
        end
    end

    set -Ux FZF_DEFAULT_OPTS "$FZF_NON_COLOR_OPTS"\
    " --color=bg+:$color01,bg:$color00,spinner:$color0C,hl:$color0D"\
    " --color=fg:$color04,header:$color0D,info:$color0A,pointer:$color0C"\
    " --color=marker:$color0C,fg+:$color06,prompt:$color0A,hl+:$color0D"
    set -Ux FZF_CTRL_T_OPTS "--preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {}'"
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
  if not contains $HOME/.asdf/shims $fish_user_paths
    and test (uname) = "Darwin"
    echo "Loading ASDF"
    set -Ux ASDF_DIR (brew --prefix asdf)
    set -l asdf_data_dir (
      if test -n "$ASDF_DATA_DIR"; echo $ASDF_DATA_DIR;
      else; echo $HOME/.asdf; end)

    # Add asdf to PATH
    set -l asdf_bin_dirs $ASDF_DIR/bin $ASDF_DIR/shims $asdf_data_dir/shims
    for x in $asdf_bin_dirs
      if test -d $x
        and not contains $x $fish_user_paths
        set -Up fish_user_paths $x
      end
    end
  end

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

