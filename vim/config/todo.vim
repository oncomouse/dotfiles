" This is many of the commands from taskpaper.vim but set to load on my
" todo.txt file and using my done notation. Also, turns off all the
" formatting.
augroup todo
  autocmd!
  " Mark a task as done:
  autocmd BufRead,BufNewFile todo.* nnoremap <buffer> <leader>td A X<esc>
  autocmd BufRead,BufNewFile todo.* vnoremap <buffer> <leader>td A X<esc>
  " Go To Project
  autocmd BufRead,BufNewFile todo.* nnoremap <buffer> <leader>tg :call dotfiles#todo#GoToProject()<CR>
  " Search for done tasks:
  autocmd BufRead,BufNewFile todo.* nnoremap <buffer> <leader>t/ / X$<CR>
  " Go To Next Project:
  autocmd BufRead,BufNewFile todo.* nnoremap <buffer> <leader>tj :call dotfiles#todo#NextProject()<CR>
  " Go To Previous Project:
  autocmd BufRead,BufNewFile todo.* nnoremap <buffer> <leader>tk :call dotfiles#todo#PrevProject()<CR>
augroup END
" # vim:foldmethod=marker
