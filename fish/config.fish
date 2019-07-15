# Set $TERM for ssh to something not custom
# function ssh; env TERM="xterm-256color" /usr/local/bin/ssh $argv; end
# Universal ignore for ag
function ag; /usr/local/bin/ag --path-to-ignore ~/.ignore --hidden $argv; end
# SSH to Dreamhost:
function pilsch.com; ssh eschaton@birkenfeld.dreamhost.com; end
# Load RBEnv
if command -sq rbenv
  status --is-interactive; and source (rbenv init - | sed 's/setenv/set -gx/' | psub)
end

# Use vim as EDITOR:
set -gx EDITOR /usr/local/bin/vim

# Configure FZF to us Ag:
set -gx FZF_DEFAULT_COMMAND "fd --type f --color=always --hidden"
set -gx FZF_CTRL_T_COMMAND "$FZF_DEFAULT_COMMAND"
set -gx FZF_ALT_C_COMMAND "fd --type d --color=always . $HOME"
set -gx FZF_DEFAULT_OPTS "
  --ansi
  --color bg:#1B2B34,fg:#C0C5CE,bg+:#4F5B66,fg+:#D8DEE9,hl:#99C794,hl+:#99C794
  --color info:108,prompt:109,spinner:108,pointer:168,marker:#EC5f67
  --bind='ctrl-o:execute(open {})+abort'
  --bind='ctrl-e:execute(code {})+abort'
  "
set -gx FZF_CTRL_T_OPTS "
  --preview-window 'right:60%' --preview 'bat --color=always --style=header,grid --line-range :300 {}'
  "

# Local paths:
if test -d ~/.fzf
  set -g fisher_user_paths $fish_user_paths ~/.fzf/bin
end
if test -d $HOME/bin
  set -g fisher_user_paths $fish_user_paths $HOME/bin
end
if test -d $HOME/go/bin
  set -g fisher_user_paths $fish_user_paths $HOME/go/bin
end
if test -d $HOME/.local/bin
  set -g fisher_user_paths $fish_user_paths $HOME/.local/bin
end
# set -g fish_user_paths $HOME/bin $HOME/.local/bin $HOME/go/bin

# Set emoji width:
set -g fish_emoji_width 2

# Anaconda Setup:
#set -gx PATH /anaconda3/bin $PATH
#source /anaconda3/etc/fish/conf.d/conda.fish

# Sets up Rust's Cargo thing:
if test -e $HOME/.cargo/env
  source $HOME/.cargo/env
end

# Setup virtualenv support Fish:
set -g VIRTUALFISH_DEFAULT_PYTHON (which python3)
if python3 -c 'import pkgutil; import sys; sys.exit(0) if pkgutil.find_loader("virtualfish") else sys.exit(1)'
  eval (python3 -m virtualfish)
end

# Setup Fuck:
if command -sq thefuck
  thefuck --alias | source
end
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

# Setup NPM:
if command -sq nodenv
  status --is-interactive; and source (nodenv init -|psub)
end

# Setup Kitty:
if command -sq kitty
  kitty + complete setup fish | source
end

# Fasd Aliases:
if command -sq nvim
  alias v='f -e nvim'
else
  alias v='f -e vim'
end

# Other Command Aliases:
alias cat='bat'
alias top='sudo htop'
alias ls='exa --group-directories-first'
