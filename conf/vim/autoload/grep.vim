let s:grep_job = get(s:, 'grep_job', -1)
let s:grep_output = get(s:, 'grep_output', 0)
let s:grep_qf = get(s:, 'grep_qf', 0)

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
          \'text': join(parts[3:], ':'),
          \}
  catch
    return ''
  endtry
endfunction

function! grep#stop() abort
  if s:grep_job >= 0
    call dotfiles#job#stop(s:grep_job)
  endif
endfunction

function! s:grep_done(job, data, ...) abort
  let s:grep_job = -1
  let s:grep_output = 0
endfunction

function! s:grep_output(job, data, ...) abort
  if s:grep_qf
    call setqflist(map(a:data, function('s:extract_qf')), s:grep_output ? 'a': 'r')
  else
    call setloclist(winnr(), map(a:data, function('s:extract_qf')), s:grep_output ? 'a': 'r')
  endif
  if s:grep_output == 0
    call grep#open_list(s:grep_qf)
    let s:grep_output = 1
  endif
endfunction

function! s:grep_launch(qf, query) abort
  if has('nvim')
    let s:grep_qf = a:qf
    let s:grep_output = 0
    if s:grep_job < 0
      call grep#stop()
    endif
    let s:grep_job = jobstart(join([&grepprg] + [expand(fnameescape(a:query))], ' '), {
          \'on_stdout': function('s:grep_output'),
          \'on_exit': function('s:grep_done'),
          \})
  else
    return system(join([&grepprg] + [expand(fnameescape(a:query))], ' '))
  endif
endfunction

function! grep#lgrep(...) abort
  return s:grep_launch(0, join(a:000, ' '))
endfunction

function! grep#grep(...) abort
  return s:grep_launch(1, join(a:000, ' '))
endfunction

function! grep#open_list(qf) abort
  let l:list = a:qf ? getqflist() : getloclist(winnr())
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
