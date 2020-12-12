function choose_dir_with_fzf --description "Choose a directory with FZF"
  eval "fd --type d . $HOME | fzf +m $FZF_DEFAULT_OPTS $FZF_ALT_C_OPTS" | read -l result
  if test -n "$result"
    open $result
  end
end

