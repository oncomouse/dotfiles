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

# Set pywal colors, if we have them:
if [[ -d "${HOME}/.cache/wal" ]]; then
	source "${HOME}/.cache/wal/colors.sh"
	FZF_COLORS=" --color=bg+:${color8},bg:${background},spinner:${color14},hl:${color12} --color=fg:${foreground},header:${color12},info:${background},pointer:${color14} --color=marker:${color14},fg+:${foreground},prompt:${color3},hl+:${color12}"
else
	FZF_COLORS=""
fi

# FZF defaults:
export FZF_DEFAULT_OPTS="--ansi --bind='ctrl-o:execute(open {})+abort' ${FZF_COLORS}"
export FZF_DEFAULT_COMMAND="fd -t f --follow --hidden"
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_ALT_C_COMMAND="fd --type d --follow"
export FZF_CTRL_T_OPTS="--preview-window 'right:60%' --preview 'bat --theme=wal --color=always --style=header,grid --line-range :300 {}'"
export FZF_ALT_C_OPTS="--preview 'ls --color=always {} | head -200'"
