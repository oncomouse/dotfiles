let b:ale_fixers = ['prettier']
imap <buffer> <silent> <C-S-Left> <C-o><<
imap <buffer> <silent> <C-S-Right> <C-o>>>
set formatprg=prettier\ parser=yaml
