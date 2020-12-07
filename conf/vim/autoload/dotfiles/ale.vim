scriptencoding utf-8
let s:ichecking = get(g:, 'dotfiles#ale#indicator_checking', '…')
let s:iinfos = get(g:, 'dotfiles#ale#indicator_infos', 'I:')
let s:iwarnings = get(g:, 'dotfiles#ale#indicator_warnings', 'W:')
let s:ierrors = get(g:, 'dotfiles#ale#indicator_errors', 'E:')
let s:iok = get(g:, 'dotfiles#ale#indicator_ok', 'OK')
let s:ale_enabled = get(b:, 'ale_enabled', get(g:, 'ale_enabled', 0))

function! dotfiles#ale#aleDiagnostics() abort
  if !exists(':ALEEnable') || !s:ale_enabled
    return { 'error': 0, 'style_error': 0, 'total': 0 }
  endif
  let l:counts = ale#statusline#Count(bufnr(''))
  return l:counts
endfunction

function! dotfiles#ale#warnings() abort
  if s:ale_enabled == 0
    return ''
  endif
  let l:linters = len(ale#linter#Get(&filetype))
  if l:linters == 0
    return ''
  endif
  if ale#engine#IsCheckingBuffer(bufnr(''))
    return ''
  endif
  let l:ale_counts = dotfiles#ale#aleDiagnostics()
  let l:warnings = l:ale_counts.total - (l:ale_counts.error + l:ale_counts.style_error)
  return l:warnings == 0 ? '' : printf(s:iwarnings . '%d', l:warnings)
endfunction

function! dotfiles#ale#errors() abort
  if s:ale_enabled == 0
    return ''
  endif
  let l:linters = len(ale#linter#Get(&filetype))
  if l:linters == 0
    return ''
  endif
  if ale#engine#IsCheckingBuffer(bufnr(''))
    return ''
  endif
  let l:ale_counts = dotfiles#ale#aleDiagnostics()
  let l:errors = l:ale_counts.error + l:ale_counts.style_error
  return l:errors == 0 ? '' : printf(s:ierrors . '%d', l:errors)
endfunction

function! dotfiles#ale#ok() abort
  if s:ale_enabled == 0
    return ''
  endif
  let l:linters = len(ale#linter#Get(&filetype))
  if l:linters == 0
    return ''
  endif
  if ale#engine#IsCheckingBuffer(bufnr(''))
    return ''
  endif
  let l:ale_counts = dotfiles#ale#aleDiagnostics()
  return l:ale_counts.total == 0 ? s:iok : ''
endfunction

function dotfiles#ale#checking() abort
  if s:ale_enabled == 0
    return ''
  endif
  let l:linters = len(ale#linter#Get(&filetype))
  if l:linters == 0
    return ''
  endif
  return ale#engine#IsCheckingBuffer(bufnr('')) ? s:ichecking : ''
endfunction

