" =========================================================
" Adapted from Coc.nvim:
" =========================================================
function s:jumpTo(line, character) abort
  " let content = getline(a:line + 1)
  " let pre = strcharpart(content, 0, a:character)
  " let col = strlen(pre) + 1
  call cursor(a:line, a:character)
endfunction
function! s:execute(cmd)
  silent exe a:cmd
  if &filetype ==# ''
    filetype detect
  endif
  if !has('nvim')
    redraw!
  endif
endfunction
" =========================================================
" Adapted from SpaceVim
" =========================================================
function! s:location_list_to_grep(v) abort
  return a:v.bufnr . ':' . a:v.lnum . ':' . a:v.col . ':' . a:v.text
endfunction
function! s:open_location_item(e) abort
  let line = a:e
  let bufnr = fnameescape(split(line, ':\d\+:')[0])
  let linenr = matchstr(line, ':\d\+:')[1:-2]
  let colum = matchstr(line, '\(:\d\+\)\@<=:\d\+:')[1:-2]
  if bufnr == bufnr('')
    call s:jumpTo(linenr, colum)
  else
    call s:execute('buffer +call cursor(' . string(linenr) . ',' . string(colum) . ') ' . string(bufnr))
  endif
  " exe 'e ' . filename
  " call cursor(linenr, colum)
endfunction
function! s:location_list() abort
  let s:source = 'location_list'
  function! s:get_location_list() abort
    return map(getloclist(0), 's:location_list_to_grep(v:val)')
  endfunction
  call fzf#run(fzf#wrap('location_list', {
        \ 'source':  reverse(<sid>get_location_list()),
        \ 'sink':    function('s:open_location_item'),
        \ 'options': '--reverse',
        \ 'down' : '40%',
        \ }))
endfunction
function! dotfiles#autocomplete#fzf#init()
  command! LocationList call s:location_list()
endfunction
