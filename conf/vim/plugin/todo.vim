" This is many of the commands from taskpaper.vim but set to load on my
" todo.txt file and using my done notation:
function! s:map_todo() abort
  " Mark A Task As Done:
  nnoremap <silent><buffer> <leader>td :call todo#toggle_done()<CR>
  vnoremap <silent><buffer> <leader>td :call todo#toggle_done()<CR>
  " Go To Project:
  nnoremap <silent><buffer> <leader>tg :call todo#goto_project()<CR>
  " Search For Done Tasks:
  nnoremap <silent><buffer> <leader>t/ / X$<CR>
  " Go To Next Project:
  nnoremap <silent><buffer> ]t :call todo#next_project()<CR>
  " Go To Previous Project:
  nnoremap <silent><buffer> [t :call todo#prev_project()<CR>
  " Ctrl+Shift+<-/-> indents
  imap <buffer> <silent> <C-S-Left> <C-o><<
  imap <buffer> <silent> <C-S-Right> <C-o>>>
endfunction

if has_key(g:, 'enable_todo')
  augroup todo
    autocmd!
    " Load For Any Todo Files:
    autocmd BufRead,BufNewFile todo.* call s:map_todo()
    " Load For VimWiki:
    autocmd FileType vimwiki call s:map_todo()
  augroup END
endif
