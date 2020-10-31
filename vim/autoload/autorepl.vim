let g:autorepl_commands = get(g:, 'autorepl_commands', {})
let g:autorepl_jobs = get(g:, 'autorepl_jobs', {})
function! s:start(command, ...) abort
  let interactive = get(a:, 001, 0)
  if interactive
    enew
    set nonumber
    set norelativenumber
    let job_id = termopen(a:command)
    bp
    return job_id
  endif
  return jobstart(a:command)
endfunction
function! autorepl#start(ft) abort
  if !(has_key(g:autorepl_commands, a:ft))
    return
  endif
  let job_id = get(g:autorepl_jobs, a:ft, -1)
  if job_id < 0
    let job = g:autorepl_commands[a:ft]
    let job_type = type(job)
    if job_type == v:t_dict
      let g:autorepl_jobs[a:ft] = s:start(job['command'], get(job, 'interactive', 0))
    else
      let g:autorepl_jobs[a:ft] = s:start(job)
    endif
  endif
  " Signal slime of our job_id:
  let b:slime_config = {'jobid': g:autorepl_jobs[a:ft]}
endfunction
