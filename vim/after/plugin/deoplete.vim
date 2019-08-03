if &runtimepath =~# 'deoplete'
  function! SmartTab() abort
    let l:emmetTypes = ['css', 'elm', 'haml', 'html', 'jade', 'less', 'sass', 'scss', 'slim']
    if index(l:emmetTypes, &filetype) >= 0
      return emmet#expandAbbrIntelligent("\<tab>")
    else
      return "\<tab>"
    endif
  endfunction
  imap <expr><TAB> pumvisible() ? "\<C-n>" : SmartTab()
  imap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
  " exec 'imap <expr><CR> pumvisible() ? deoplete#close_popup() : "\<CR>\<Plug>DiscretionaryEnd\<Plug>AutoPairsReturn"'
  " imap <expr><Esc> pumvisible() ? deoplete#cancel_popup() : "\<ESC>"
endif
