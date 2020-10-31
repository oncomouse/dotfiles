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
    call execute(':term '.command)
    let job_id = &channel
    if !focus
      bp
    endif
    return job_id
  endif
  return jobstart(a:command)
endfunction
function! autorepl#start(ft, ...) abort
  if !(has_key(g:autorepl_commands, a:ft))
    return
  endif
  let auto = get(a:, 001, 1)
  let job_id = get(g:autorepl_jobs, a:ft, -1)
  if job_id < 0
    let job = g:autorepl_commands[a:ft]
    let job_type = type(job)
    if job_type == v:t_dict
      if auto == 0 || get(job, 'auto', 1)
        let g:autorepl_jobs[a:ft] = s:start(job['command'], get(job, 'interactive', 0), !auto)
      endif
    else
      let g:autorepl_jobs[a:ft] = s:start(job)
    endif
  endif
  if auto == 0 || get(job, 'auto', 1)
    " Signal slime of our job_id:
    let b:slime_config = {'jobid': g:autorepl_jobs[a:ft]}
  endif
endfunction

function! autorepl#open_repl() abort
  call autorepl#start(&filetype, 0)
endfunction
