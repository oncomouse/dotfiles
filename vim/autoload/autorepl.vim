let g:autorepl_commands = get(g:, 'autorepl_commands', {})
let g:autorepl_jobs = get(g:, 'autorepl_jobs', {})
let g:autorepl_no_line_numbers = get(g:, 'autorepl_no_line_numbers', 1)
function! s:start_repl(command, ...) abort
  " Is the REPL interactive:
  let interactive = get(a:, 001, 0)
  " Is the REPL going to retain focus:
  let focus = get(a:, 002, 0)
  let command = type(a:command) == v:t_list ? join(a:command, ' ') : a:command
  if interactive
    " Open the terminal command:
    call execute(':terminal '.(has('nvim') ? '' : '++curwin ++kill=term ').expand(command))
    " This gets sourced I think before this gets set for terminals:
    if g:autorepl_no_line_numbers
      set nonumber
      set norelativenumber
    endif
    let output = has('nvim') ? &channel : buffer_number()
    if !focus
      bp
    endif
    " Return the identifier we need for slime:
    return output
  endif
  " If not interactive, start the job:
  return has('nvim') ? jobstart(a:command) : job_getchannel(job_start(a:command))
endfunction
function! s:load_repl(ft,auto) abort
  " Don't load if we don't have a repl defined:
  if !(has_key(g:autorepl_commands, a:ft))
    return
  endif
  let job_id = get(g:autorepl_jobs, a:ft, -1)
  let start = 0
  if job_id < 0
    let job = g:autorepl_commands[a:ft]
    if type(job) != v:t_dict
      let job = {'command': job, 'interactive': 0, 'auto': 1}
    else
      let job = extend({'interactive': 0, 'auto': 1}, job)
    endif
    if a:auto == 0 || job.auto
      let g:autorepl_jobs[a:ft] = s:start_repl(job.command, job.interactive, !a:auto)
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
