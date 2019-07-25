let s:iinfos = get(g:, 'lightline#combined#prefix_infos', 'I:')
let s:iwarnings = get(g:, 'lightline#combined#prefix_warnings', 'W:')
let s:ierrors = get(g:, 'lightline#combined#prefix_errors', 'E:')
let s:iok = get(g:, 'lightline#combined#prefix_ok', 'OK')
let s:indicator_checking = get(g:, 'lightline#combined#indicator_checking', 'Linting...')
let g:language_client_started = get(g:, 'language_client_started', 0)

function! lightline#combined#lspStarted() abort
    let g:language_client_started = 1
    call lightline#update()
endfunction

function! lightline#combined#lspStopped() abort
    let g:language_client_started = 0
    call lightline#update()
endfunction

function! lightline#combined#notStarted() abort
  return (g:language_client_started == 0) ? s:indicator_notstarted : ''
endfunction

function! lightline#combined#warnings() abort
  let l:neomake_warnings = !exists(":Neomake") ? 0 : get(neomake#statusline#LoclistCounts(), 'W', 0)
  let l:lsp_warning_no = LanguageClient_serverStatus() == 1 ? 0 : len(filter(getqflist(), 'v:val["type"] == "W"'))
  let l:warnings = l:neomake_warnings + l:lsp_warning_no
  return l:warnings == 0 ? '' : printf(s:iwarnings . '%d', l:warns)
endfunction

function! lightline#combined#errors() abort
  let l:neomake_errors = !exists(":Neomake") ? 0 : get(neomake#statusline#LoclistCounts(), 'E', 0)
  let l:lsp_errors = LanguageClient_serverStatus() == 1 ? 0 : len(filter(getqflist(), 'v:val["type"] == "E"'))
  let l:errors = l:neomake_errors + l:lsp_errors
  return l:errors == 0 ? '' : printf(s:ierrors . '%d', l:errors)
endfunction

function! lightline#combined#infos() abort
  let l:neomake_infos = !exists(":Neomake") ? 0 : get(neomake#statusline#LoclistCounts(), 'I', 0)
  let l:lsp_info_no = LanguageClient_serverStatus() == 1 ? 0 : len(filter(getqflist(), 'v:val["type"] == "I"'))
  let l:infos = l:neomake_infos + l:lsp_info_no
  return l:infos == 0 ? '' : printf(s:iinfos . '%d', l:infos)
endfunction

function! lightline#combined#ok() abort
  let l:lsp_count = LanguageClient_serverStatus() == 1 ? -1 : len(getqflist())
  let l:neomake_list = !exists(":Neomake") ? -1 : len(neomake#statusline#LoclistCounts())
  if l:lsp_count < 0 && l:neomake_list < 0
    return ''
  elseif  l:lsp_count < 0
    return l:neomake_list == 0 ? s:iok : ''
  elseif l:neomake_list < 0
    return l:lsp_count == 0 ? s:iok : ''
  endif
  return l:lsp_count + l:neomake_list == 0 ? s:iok : ''
endfunction
