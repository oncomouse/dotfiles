# Add a version of fisher that emits events
emit-fisher

# # This fixes some wonky behavior in the fisher plugin for fasd
function __patch-fasd
  printf "%s\n" \
  "function __fasd_run -e fish_preexec -d 'fasd takes record of the directories changed into'" \
  "  if test \$argv[1] != 'exit'" \
  "    command fasd --proc (command fasd --sanitize '\$argv') > '/dev/null' 2>&1 &" \
  "  end" \
  "end" > ~/.config/fish/conf.d/__fasd_run.fish
  source ~/.config/fish/conf.d/__fasd_run.fish
end
function __install-fix-fasd --on-event fisher_install;__patch-fasd;end
function __add-fix-fasd --on-event fisher_add;__patch-fasd;end
function __rm-fix-fasd --on-event fisher_rm;__patch-fasd;end

# Universal ignore for ag
function ag; /usr/bin/env ag --path-to-ignore ~/.ignore --hidden $argv; end
# SSH to Dreamhost:
function pilsch.com; ssh eschaton@birkenfeld.dreamhost.com; end

# Load RBEnv
# if command -sq rbenv
#   set -gx RUBY_CONFIGURE_OPTS --with-openssl-dir=(brew --prefix openssl@1.1)
#   status --is-interactive; and source (rbenv init --no-rehash - | sed 's/setenv/set -gx/' | psub)
# end

# Vim related aliases:
if command -sq nvim
  set -gx EDITOR (which nvim)
else
  set -gx EDITOR (which vim)
end

# Configure FZF:
set -gx FZF_DEFAULT_COMMAND "fd --type f --color=always --hidden"
set -gx FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
set -gx FZF_ALT_C_COMMAND "fd --type d --color=always . $HOME"
set -gx FZF_DEFAULT_OPTS "
  --ansi
  --bind='ctrl-o:execute(open {})+abort'
  --bind='ctrl-e:execute(code {})+abort'
  "

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

set -U FZF_DEFAULT_OPTS "$FZF_NON_COLOR_OPTS"\
" --color=bg+:$color01,bg:$color00,spinner:$color0C,hl:$color0D"\
" --color=fg:$color04,header:$color0D,info:$color0A,pointer:$color0C"\
" --color=marker:$color0C,fg+:$color06,prompt:$color0A,hl+:$color0D"
set -gx FZF_CTRL_T_OPTS "
  --preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {}'
  "

# Local paths:
function add_to_user_paths -a dir
  if not contains $dir $fish_user_paths
    and test -d $dir
    set -U fish_user_paths $fish_user_paths $dir
  end
end
add_to_user_paths ~/.fzf/bin
add_to_user_paths ~/bin
add_to_user_paths ~/go/bin
add_to_user_paths ~/.local/bin
add_to_user_paths /usr/local/sbin

# # Set emoji width:
# set -g fish_emoji_width 2

# Sets up Rust's Cargo thing:
if test -e $HOME/.cargo/env
  source $HOME/.cargo/env
end

# Configure ASDF:

if not contains $HOME/.asdf/shims $fish_user_paths
  echo "Loading ASDF"
  set -x ASDF_DIR (brew --prefix asdf)
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

# Setup virtualenv support Fish:
# if not set -q VIRTUALFISH_DEFAULT_PYTHON
#   set -U VIRTUALFISH_DEFAULT_PYTHON (which python)
#   echo "Setting up virtualfish"
#   if python -c 'import pkgutil; import sys; sys.exit(0) if pkgutil.find_loader("virtualfish") else sys.exit(1)'
#     eval (python -m virtualfish | sed -e "s/set \-g/set -U/g")
#   end
# end

# Setup Fuck:
if command -sq thefuck
  thefuck --alias | source
end

# Setup NPM:
# if command -sq nodenv
#   status --is-interactive; and source (nodenv init -|psub)
# end

# Setup Kitty:
if command -sq kitty
  kitty + complete setup fish | source
end

# if [ -x ~/.local/bin/nvim-osx64/bin/nvim ]
#   function nvim;~/.local/bin/nvim-osx64/bin/nvim $argv;end
# end

# Fasd Aliases:
if command -sq nvim
  function v;f -t -e nvim $argv;end
else
  function v;f -t -b viminfo -e vim $argv;end
end

# Other Command Aliases:
function cat;bat --paging=never $argv;end
function top;htop $argv;end
function ls;exa --group-directories-first $argv;end
function vim;nvim -u ~/dotfiles/vim/vimrc-server $argv;end

# Colors:
set fish_color_cwd cyan
