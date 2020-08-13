function! dotfiles#autocomplete#clap#init() abort
  " Display location list:
  nmap <silent> <leader>d :<C-u>Clap loclist<CR>
  " Old FZF Interface:
  command!      -bang -nargs=? -complete=dir FZF    exe 'Clap files ++query='.<q-args>
  command! Buffers :exe 'Clap buffers'
  command! Windows :exe 'Clap windows'
  command! BLines :exe 'Clap lines'
  command! History :exe 'Clap command_history'
  nnoremap <silent> <leader>y  :<C-u>Clap yanks<CR>
  " Implement Ag
  command! -nargs=+ -complete=custom,s:GrepArgs Ag exe 'Clap grep2 ++query='.<q-args>
  vnoremap <silent> <leader>/ :<C-u>Clap grep2 ++query=@visual<CR>
endfunction
