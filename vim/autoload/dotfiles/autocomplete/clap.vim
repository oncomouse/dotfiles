function! dotfiles#autocomplete#clap#init() abort
  " Old FZF Interface:
  command! -bang -nargs=? -complete=dir Files exe 'Clap files ++query='.<q-args>
  command! Buffers :exe 'Clap buffers'
  command! Windows :exe 'Clap windows'
  command! BLines :exe 'Clap lines'
  command! History :exe 'Clap command_history'
  command! LocationList :exe 'Clap loclist'
  command! QuickfixList :exe 'Clap quickfixlist'
  command! Yanks :exe 'Clap yanks'
endfunction
