function! visualat#execute_macro_over_visual_range() abort
  echo '@'.getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction
