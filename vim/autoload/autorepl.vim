let g:autorepl_commands = get(g:, 'autorepl_commands', {})
let g:autorepl_jobs = get(g:, 'autorepl_jobs', {})
function! autorepl#start(ft) abort
  if has_key(g:autorepl_commands, a:ft) && get(g:autorepl_jobs, a:ft, -1) < 0
    if has('nvim')
      let g:autorepl_jobs[a:ft] = jobstart(g:autorepl_commands[a:ft])
    endif
  endif
endfunction
