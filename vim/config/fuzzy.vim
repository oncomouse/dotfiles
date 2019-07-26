" FZF {{{
  " Highlight file with <shift>-<tab>; press the follow to open:
  let g:fzf_action = {
      \ 'ctrl-s': 'split',
      \ 'ctrl-v': 'vsplit',
      \ 'ctrl-t': 'tabnew',
      \ 'ctrl-e': 'edit',
      \ }
  nnoremap <silent> <c-p> :FZF<CR>
  nnoremap <silent> <leader>F :FZF ~<CR>
  let g:fzf_nvim_statusline = 0 " disable statusline overwriting

  " Close the quick fix window:
  nnoremap <silent> <leader>cc :cwindow<CR>
  nnoremap <silent> <leader>ll :lwindow<CR>

  nnoremap <silent> <leader>a :Buffers<CR>
  nnoremap <silent> <leader>A :Windows<CR>
  nnoremap <silent> <leader>l :BLines<CR>
  " nnoremap <silent> <leader>o :BTags<CR>
  " nnoremap <silent> <leader>O :Tags<CR>
  nnoremap <silent> <leader>? :History<CR>
  nnoremap <silent> <leader>/ :execute 'Ag ' . input('Ag/')<CR>
  nnoremap <silent> K :call dotfiles#ag#SearchWordWithAg()<CR>
  vnoremap <silent> K :call dotfiles#ag#SearchVisualSelectionWithAg()<CR>
  " nnoremap <silent> <leader>gl :Commits<CR>
  " nnoremap <silent> <leader>ga :BCommits<CR>
  " nnoremap <silent> <leader>ft :Filetypes<CR>
  
  " Access yank history:
  let g:neoyank#file = $HOME.'/.vim/yankring.txt'
  nnoremap <leader>rr :FZFNeoyank *<cr>
  nnoremap <leader>RR :FZFNeoyank * P<cr>
  vnoremap <leader>rr :FZFNeoyankSelection<cr>
  " Complete file name:
  imap <C-x><C-f> <plug>(fzf-complete-file-ag)
  " Complete file line:
  imap <C-x><C-l> <plug>(fzf-complete-line)
  "
" }}}

" # vim:foldmethod=marker
