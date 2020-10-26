function! grep#grep(...)
  return system(join([&grepprg] + [expand(fnameescape(join(a:000, ' ')))], ' '))
endfunction

function! grep#open_list(qf) abort
  let l:list = a:qf ? getqflist() : getloclist()
  let l:pfx = a:qf ? 'c' : 'l'
  if len(l:list) == 0
    redraw
    echohl ErrorMsg
    echo 'No Results'
    echohl NONE
  else
    exec(l:pfx.'window')
  endif
endfunction
