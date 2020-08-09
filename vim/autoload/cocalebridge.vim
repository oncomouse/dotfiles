function! cocalebridge#getDiagnostics(buffer) abort
  if buflisted(a:buffer)
    call ale#other_source#StartChecking(a:buffer, 'coc')
    call CocActionAsync('diagnosticList', {_err, results -> s:handle_results(a:buffer, results)})
  endif
endfunction
function! s:processList(idx, list) abort
  let l:list = {
  \ 'lnum': a:list['location']['range']['start']['line'],
  \ 'end_lnum': a:list['location']['range']['start']['line'],
  \ 'col': a:list['location']['range']['start']['character'],
  \ 'end_col': a:list['location']['range']['end']['character'],
  \ 'text': a:list['message'],
  \ 'type': a:list['severity'][0],
  \}
  return l:list
endfunction
function! s:handle_results(buffer, results) abort
    let l:output = map(a:results, function('s:processList'))
    call ale#other_source#ShowResults(a:buffer, 'coc', l:output)
endfunction
