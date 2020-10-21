" =========================================================
" Adapted from SpaceVim
" =========================================================
function! s:list_to_grep(v) abort
  let l:bufname = a:v.bufnr == bufnr('') ? '' : fnamemodify(bufname(a:v.bufnr), ':~:.') . ':'
  return l:bufname . a:v.lnum . ':' . a:v.col . ':' . a:v.text
endfunction
function! s:open_list_item(e) abort
  let l:line = a:e
  if !(matchstr(l:line[0], '\d'))
    let l:bufname = fnameescape(split(l:line, ':')[0])
    let l:linenr = matchstr(l:line, ':\d\+:')[1:-2]
    let l:colum = matchstr(l:line, '\(:\d\+\)\@<=:\d\+:')[1:-2]
    execute 'edit' l:bufname
  else
    let parts = split(l:line, ':')
    let l:linenr = parts[0]
    let l:colum = parts[1]
  endif
  call cursor(l:linenr, l:colum)
endfunction
function! s:fzf_list(name, is_quickfix) abort
  let source = a:is_quickfix ? getqflist() : getloclist(0)
  call fzf#run(fzf#wrap(a:name, {
        \ 'source': map(source, 's:list_to_grep(v:val)'),
        \ 'sink': function('s:open_list_item'),
        \ }))
endfunction
function! s:location_list() abort
  let name = 'location_list'
  call s:fzf_list(name, v:false)
endfunction
function! s:quickfix() abort
  let name = 'quickfix'
  call s:fzf_list(name, v:true)
endfunction
" =========================================================
" From junegunn's dotfiles
" =========================================================
function! FloatingFZF()
  let buf = nvim_create_buf(v:false, v:true)
  call setbufvar(buf, '&signcolumn', 'no')

  let width = float2nr(&columns - (&columns * 2 / 10))
  let height = &previewheight "&lines - 3
  let y = float2nr((&lines - height) / 2)
  let x = float2nr((&columns - width) / 2)

  let opts = {
        \ 'relative': 'editor',
        \ 'row': y,
        \ 'col': x,
        \ 'width': width,
        \ 'height': height
        \ }

  call nvim_open_win(buf, v:true, opts)
endfunction
" =========================================================
function! dotfiles#autocomplete#fzf#init()
  command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--reverse', '--info=inline']}), <bang>0)
  command! LocationList call s:location_list()
  command! QuickfixList call s:quickfix()
  command! Yanks exe 'FZFNeoyank'
  nnoremap <leader>Y :FZFNeoyank " P<cr>
  vnoremap <leader>y :FZFNeoyankSelection<cr>
  let $FZF_DEFAULT_OPTS .= ' --reverse'
  " let $FZF_DEFAULT_COMMAND = 'fd --type f --hidden'
  if has('nvim') && g:dotfiles_mode ==# 'desktop'
    let g:fzf_layout = { 'window': 'call FloatingFZF()' }
  endif
  let g:fzf_action = {
    \ 'ctrl-s': 'split',
    \ 'ctrl-v': 'vsplit',
    \ 'ctrl-t': 'tabnew',
    \ 'ctrl-e': 'edit',
    \ }
  let g:fzf_nvim_statusline = 0 " disable statusline overwriting
  " Complete file name:
  " imap <C-x><C-f> <plug>(fzf-complete-file-ag)
  " Complete file line:
  " imap <C-x><C-l> <plug>(fzf-complete-line)
endfunction
