if &rtp =~ 'deoplete'
  imap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
  imap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
  imap <expr><CR> pumvisible() ? deoplete#close_popup() : "\<CR>\<Plug>AutoPairsReturn"
  " imap <expr><Esc> pumvisible() ? deoplete#cancel_popup() : "\<ESC>"
endif
