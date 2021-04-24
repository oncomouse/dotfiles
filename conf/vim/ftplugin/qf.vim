if has_key(g:, 'dotfiles_mode')
  setlocal statusline=%!dotfiles#statusline#statusline()
endif
