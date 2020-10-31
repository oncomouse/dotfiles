let g:autorepl_commands = get(g:, 'autorepl_commands', {})
let g:autorepl_jobs = get(g:, 'autorepl_jobs', {})
function! s:start(command, ...) abort
  let interactive = get(a:, 001, 0)
  let focus = get(a:, 002, 0)
  let command = type(a:command) == v:t_list ? join(a:command, ' ') : a:command
  if interactive
    " This gets sourced I think before this gets set for terminals:
    set nonumber
    set norelativenumber
    call execute(':terminal '.(has('nvim') ? '' : '++curwin ++kill=term ').expand(command))
    let job_id = has('nvim') ? &channel : buffer_number()
    if !focus
      bp
    endif
    return job_id
  endif
  return has('nvim') ? jobstart(a:command) : job_getchannel(job_start(a:command))
endfunction
function! s:load_repl(ft,auto) abort
  if !(has_key(g:autorepl_commands, a:ft))
    return
  endif
  let job_id = get(g:autorepl_jobs, a:ft, -1)
  let start = 0
  if job_id < 0
    let job = g:autorepl_commands[a:ft]
    let job_type = type(job)
    if job_type == v:t_dict
      if a:auto == 0 || get(job, 'auto', 1)
        let g:autorepl_jobs[a:ft] = s:start(job['command'], get(job, 'interactive', 0), !a:auto)
        let start = 1
      endif
    else
      let g:autorepl_jobs[a:ft] = s:start(job)
      let start = 1
    endif
  endif
  if job_id >= 0 || start
    " Signal slime of our job_id:
    let b:slime_config = {'jobid': g:autorepl_jobs[a:ft], 'bufnr': g:autorepl_jobs[a:ft]}
  endif
endfunction
function! autorepl#start(ft) abort
  call s:load_repl(a:ft, 1)
endfunction

function! autorepl#open_repl() abort
  call s:load_repl(&filetype, 0)
endfunction
