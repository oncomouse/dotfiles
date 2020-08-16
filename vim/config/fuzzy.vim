" FZF Colors {{{
" let g:fzf_colors =
"   \ { 'fg':      ['fg', 'Normal'],
"   \ 'bg':      ['bg', 'Normal'],
"   \ 'hl':      ['fg', 'Comment'],
"   \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
"   \ 'bg+':     ['bg', 'Visual', 'CursorColumn'],
"   \ 'hl+':     ['fg', 'Statement'],
"   \ 'info':    ['fg', 'MoreMessage'],
"   \ 'border':  ['fg', 'Ignore'],
"   \ 'prompt':  ['fg', 'Special'],
"   \ 'pointer': ['fg', 'Exception'],
"   \ 'marker':  ['fg', 'Keyword'],
"   \ 'spinner': ['fg', 'Label'],
"   \ 'header':  ['fg', 'Comment'] } 
" }}}
" FZF Configuration {{{
  " Highlight file with <shift>-<tab>; press the follow to open:
  let g:fzf_action = {
    \ 'ctrl-s': 'split',
    \ 'ctrl-v': 'vsplit',
    \ 'ctrl-t': 'tabnew',
    \ 'ctrl-e': 'edit',
    \ }
  let g:fzf_nvim_statusline = 0 " disable statusline overwriting
" }}}
" Fuzzy Bindings {{{
  nnoremap <silent> <c-p> :Files<CR>
  nnoremap <silent> <leader>F :Files ~<CR>
  nnoremap <silent> <leader>d :LocationList<CR>
  nnoremap <silent> <leader>a :Buffers<CR>
  nnoremap <silent> <leader>A :Windows<CR>
  nnoremap <silent> <leader>l :BLines<CR>
  nnoremap <silent> <leader>? :History<CR>
  nnoremap <silent> <leader>/ :execute 'Rg ' . input('Rg/')<CR>
  nnoremap <silent> <leader>y :Yanks<CR>
  " FZF Only at this point:
  if &runtimepath =~# 'fzf'
    " Complete file name:
    imap <C-x><C-f> <plug>(fzf-complete-file-ag)
    " Complete file line:
    imap <C-x><C-l> <plug>(fzf-complete-line)
  endif
" }}}

" # vim:foldmethod=marker
