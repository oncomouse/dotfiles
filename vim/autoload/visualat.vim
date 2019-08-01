function! visualat#ExecuteMacroOverVisualRange() abort
  echo '@'.getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction
