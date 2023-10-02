#!/usr/bin/env bash

get_tmux_option() {
  local option value default
  option="$1"
  default="$2"
  value="$(tmux show-option -gqv "$option")"

  if [ -n "$value" ]; then
    echo "$value"
  else
    echo "$default"
  fi
}

set() {
  local option=$1
  local value=$2
  tmux_commands+=(set-option -gq "$option" "$value" ";")
}

setw() {
  local option=$1
  local value=$2
  tmux_commands+=(set-window-option -gq "$option" "$value" ";")
}
main() {
  # local theme
  # theme="$(get_tmux_option "@catppuccin_flavour" "mocha")"

  # Aggregate all commands in one array
  local tmux_commands=()

  # shellcheck source=catppuccin-frappe.tmuxtheme
  source /dev/stdin <<<"$(sed -e "/^[^#].*=/s/^/local /" "${HOME}/.cache/wal/wal.tmuxtheme")"

  # Standard Status Colors
  set status-fg $thm_fg
  set status-bg $thm_bg

  # Pane Borders
  set pane-border-style "bg=default fg=${thm_gray}"
  set pane-active-border-style "bg=default fg=${thm_black4}"

  ## Status
  # Clear Right Status
  set status-left ""
  set status-right "#[fg=$thm_fg,bg=$thm_black4] %I:%M%p #[fg=$thm_bg,bg=$thm_green] #S "

  # Current Window Label (w/ Teal title):
  set window-status-format "#[fg=$thm_bg,bg=$thm_blue] #I #[bg=default,fg=default] #W#{?window_flags,#{window_flags}, } "
  set window-status-current-format "#[fg=$thm_bg,bg=$thm_orange] #I #[bg=$thm_black4]#[fg=default] #W#{?window_flags,#{window_flags}, } "
  set window-status-separator ""

  tmux "${tmux_commands[@]}"
}

main "$@"
#vim:ft=sh
