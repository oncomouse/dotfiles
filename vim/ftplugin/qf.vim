if g:dotfiles_mode !=# 'desktop'
  setlocal statusline=%!dotfiles#statusline#statusline()
endif
