# Setup fzf
# ---------
if [[ ! "$PATH" == *${HOME}/.fzf/bin* ]]; then
  export PATH="$PATH:${HOME}/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "$( (( $+commands[brew] )) && brew --prefix)/opt/fzf/completion.zsh" 2> /dev/null
[[ $- == *i* ]] && source "/usr/share/fzf/completion.zsh" 2> /dev/null
[[ $- == *i* ]] && source "${HOME}/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
[[ -e "$( (( $+commands[brew] )) && brew --prefix)/opt/fzf/key-bindings.zsh" ]] && source "$(brew --prefix)/opt/fzf/key-bindings.zsh"
[[ -e "/usr/share/fzf/key-bindings.zsh" ]] && source "/usr/share/fzf/key-bindings.zsh"
[[ -e "${HOME}/.fzf/shell/key-bindings.zsh" ]] && source "${HOME}/.fzf/shell/key-bindings.zsh"
