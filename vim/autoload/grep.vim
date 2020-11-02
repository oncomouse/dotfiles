let g:grep_job = get(g:, 'grep_job', -1)
let g:grep_output = get(g:, 'grep_output', 0)
let g:grep_qf = get(g:, 'grep_qf', 0)
function! s:grep_error(job, data, ...) abort
  echoerr 'Err:' . string(a:data)
endfunction

function! s:extract_qf(ln, line) abort
  try
    let parts = split(a:line, ':')
    if len(parts) < 4
      return ''
    endif
    return {
          \'filename': parts[0],
          \'lnum': parts[1],
          \'col': parts[2],
          \'text': parts[3],
          \}
  catch
    return ''
  endtry
endfunction

function grep#stop() abort
  if g:grep_job >= 0
    call jobstop(g:grep_job)
  endif
endfunction

function! s:grep_done(job, data, ...) abort
  let g:grep_job = -1
  let g:grep_output = 0
endfunction

function! s:grep_output(job, data, ...) abort
  call setqflist(map(a:data, function('s:extract_qf')), g:grep_output ? 'a': 'r')
  if g:grep_output == 0
    call grep#open_list(g:grep_qf)
    let g:grep_output = 1
  endif
endfunction

function! grep#grep(...) abort
  if has('nvim')
    let g:grep_output = 0
    if g:grep_job < 0
      call grep#stop()
    endif
    let g:grep_qf = get(a:, 001, 0)
    let g:grep_job = jobstart(join([&grepprg] + [expand(fnameescape(join(a:000, ' ')))], ' '), {
          \'on_stdout': function('s:grep_output'),
          \'on_done': function('s:grep_done'),
          \})
  endif
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
