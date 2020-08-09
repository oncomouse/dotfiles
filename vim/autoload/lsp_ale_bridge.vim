function! lsp_ale_bridge#getDiagnostics(buffer) abort
  if buflisted(a:buffer)
    call ale#other_source#StartChecking(a:buffer, 'vim-lsp')
    let l:results = lsp#ui#vim#diagnostics#get_document_diagnostics(a:buffer)
    let l:output = []
    for [l:server_name, l:response] in items(l:results)
      let l:output = extend(l:output, map(l:response['response']['params']['diagnostics'], function('s:processList')))
    endfor
    call filter(l:output, 'len(keys(v:val)) > 0')
    call ale#other_source#ShowResults(a:buffer, 'vim-lsp', l:output)
  endif
endfunction
let s:diagnostic_severity = {
    \ 1: 'Error',
    \ 2: 'Warning',
    \ 3: 'Info',
    \ 4: 'Hint',
    \ }
function! s:processList(idx, list) abort
  if has_key(a:list, 'range')
    let l:list = {
    \ 'lnum': str2nr(a:list['range']['start']['line']) + 1,
    \ 'end_lnum': str2nr(a:list['range']['start']['line']) + 1,
    \ 'col': str2nr(a:list['range']['start']['character']) + 1,
    \ 'end_col': str2nr(a:list['range']['end']['character']) + 1,
    \ 'text': a:list['message'],
    \ 'type': s:diagnostic_severity[str2nr(a:list['severity'])],
    \}
    return l:list
  endif
  return {}
endfunction
