" Fuzzy Bindings {{{
  " This sets up fuzzy keybindings. We use it for any configuration framework.
  " Other completion frameworks (vim-clap, denite, coc.nvim) implement these
  " functions, so that bindings are universal.
  nnoremap <silent> <c-p> :Files<CR>
  nnoremap <silent> <leader>F :Files ~<CR>
  nnoremap <silent> <leader>q :QuickfixList<CR>
  nnoremap <silent> <leader>d :LocationList<CR>
  nnoremap <silent> <leader>a :Buffers<CR>
  nnoremap <silent> <leader>A :Windows<CR>
  nnoremap <silent> <leader>l :BLines<CR>
  nnoremap <silent> <leader>? :History<CR>
  nnoremap <silent> <leader>/ :execute 'Rg ' . input('Rg/')<CR>
  nnoremap <silent> <leader>y :Yanks<CR>
" }}}

" # vim:foldmethod=marker
