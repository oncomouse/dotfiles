function! dotfiles#autocomplete#clap#init() abort
  " Old FZF Interface:
  command! -nargs=? -complete=dir Files exe 'Clap files ++query='.<q-args>
  command! Buffers :exe 'Clap buffers'
  command! Windows :exe 'Clap windows'
  command! BLines :exe 'Clap lines'
  command! History :exe 'Clap command_history'
  command! LocationList :exe 'Clap loclist'
  command! QuickfixList :exe 'Clap quickfix'
  command! Yanks :exe 'Clap yanks'
  command! -nargs=+ -complete=custom,dotfiles#rg_args Rg exe 'Clap grep2 ++query='.<q-args>
endfunction
