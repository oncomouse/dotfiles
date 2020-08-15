function! dotfiles#autocomplete#clap#init() abort
  " Old FZF Interface:
  command! -bang -nargs=? -complete=dir Files exe 'Clap files ++query='.<q-args>
  command! Buffers :exe 'Clap buffers'
  command! Windows :exe 'Clap windows'
  command! BLines :exe 'Clap lines'
  command! History :exe 'Clap command_history'
  command! LocationList :exe 'Clap loclist'
  command! Yanks :exe 'Clap yanks'
  " Implement Ag
  command! -nargs=+ -complete=custom,s:GrepArgs Ag exe 'Clap grep2 ++query='.<q-args>
  vnoremap <silent> <leader>/ :<C-u>Clap grep2 ++query=@visual<CR>
endfunction
