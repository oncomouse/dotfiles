let s:iinfos = get(g:, 'dotfiles#combined#indicator_infos', 'I:')
let s:iwarnings = get(g:, 'dotfiles#combined#indicator_warnings', 'W:')
let s:ierrors = get(g:, 'dotfiles#combined#indicator_errors', 'E:')
let s:iok = get(g:, 'dotfiles#combined#indicator_ok', 'OK')
let s:indicator_checking = get(g:, 'dotfiles#combined#indicator_checking', 'Linting...')
let s:indicator_notstarted = s:indicator_checking
let g:language_client_started = get(g:, 'language_client_started', 0)

function! dotfiles#combined#aleDiagnostics() abort
  if !exists(':ALEEnable')
    return { 'error': 0, 'style_error': 0, 'total': 0 }
  endif
  let l:counts = ale#statusline#Count(bufnr(''))
  return l:counts
endfunction

function! dotfiles#combined#lspStarted() abort
  let g:language_client_started = 1
  " call dotfiles#update()
endfunction

function! dotfiles#combined#lspStopped() abort
  let g:language_client_started = 0
  " call dotfiles#update()
endfunction

function! dotfiles#combined#notStarted() abort
  return (g:language_client_started == 0) ? s:indicator_notstarted : ''
endfunction

function! dotfiles#combined#warnings() abort
  let l:neomake_warnings = !exists(':Neomake') ? 0 : get(neomake#statusline#LoclistCounts(), 'W', 0)
  let l:lsp_warning_no = exists('LanguageClient_serverStatus') && LanguageClient_serverStatus() == 1 ? 0 : len(filter(getqflist(), 'v:val["type"] ==# "W"'))
  let l:ale_counts = dotfiles#combined#aleDiagnostics()
  let l:ale_warnings = l:ale_counts.total - (l:ale_counts.error + l:ale_counts.style_error)
  let l:warnings = l:neomake_warnings + l:lsp_warning_no + l:ale_warnings
  return l:warnings == 0 ? '' : printf(s:iwarnings . '%d', l:warnings)
endfunction

function! dotfiles#combined#errors() abort
  let l:neomake_errors = !exists(':Neomake') ? 0 : get(neomake#statusline#LoclistCounts(), 'E', 0)
  let l:lsp_errors = exists('LanguageClient_serverStatus') && LanguageClient_serverStatus() == 1 ? 0 : len(filter(getqflist(), 'v:val["type"] ==# "E"'))
  let l:ale_counts = dotfiles#combined#aleDiagnostics()
  let l:ale_errors = l:ale_counts.error + l:ale_counts.style_error
  let l:errors = l:neomake_errors + l:lsp_errors + l:ale_errors
  return l:errors == 0 ? '' : printf(s:ierrors . '%d', l:errors)
endfunction

function! dotfiles#combined#ok() abort
  let l:lsp_count = exists('LanguageClient_serverStatus') && LanguageClient_serverStatus() == 1 ? -1 : len(getqflist())
  let l:neomake_list = !exists(':Neomake') ? -1 : len(neomake#statusline#LoclistCounts())
  let l:ale_counts = dotfiles#combined#aleDiagnostics()
  if l:lsp_count < 0 && l:neomake_list < 0
    return exists(':ALEEnable') && l:ale_counts.total == 0 ? s:iok : ''
  elseif  l:lsp_count < 0
    return l:neomake_list + l:ale_counts.total == 0 ? s:iok : ''
  elseif l:neomake_list < 0
    return l:lsp_count + l:ale_counts.total == 0 ? s:iok : ''
  endif
  return l:lsp_count + l:neomake_list + l:ale_counts.total == 0 ? s:iok : ''
endfunction
