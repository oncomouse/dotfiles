" Statusline:
" Lightline {{
  set laststatus=2
  set showtabline=2
  let g:lightline = {
        \ 'colorscheme': 'oceanicnext',
        \ 'active': {
        \   'left': [ [ 'mode', 'paste' ],
        \             [ 'gitbranch', 'gitgutter', 'readonly', 'filename', 'modified' ] ],
        \   'right': [ [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ],
        \            [ 'lineinfo' ],
        \            [ 'wordcount' ], ]
        \ },
        \ 'component_function': {
        \   'gitbranch': 'fugitive#head',
        \   'gitgutter': 'MyGitGutter',
        \   'wordcount': 'WordCount',
        \   'filetype': 'MyFiletype',
        \ },
        \ 'component_expand': {
        \   'linter_checking': 'lightline#ale#checking',
        \   'linter_warnings': 'lightline#ale#warnings',
        \   'linter_errors': 'lightline#ale#errors',
        \   'linter_ok': 'lightline#ale#ok',
        \   'buffers': 'lightline#bufferline#buffers',
        \ },
        \ 'component_type': {
        \     'linter_checking': 'left',
        \     'linter_warnings': 'warning',
        \     'linter_errors': 'error',
        \     'linter_ok': 'left',
        \     'buffers': 'tabsel',
        \ },
        \ 'separator': { 'left': '', 'right': '' },
        \ 'subseparator': { 'left': '', 'right': '' }
        \ }
  let g:lightline.tab_component_function = {
        \   'filetype': 'MyTabFiletype',
        \   'mytabname': 'MyTabName',
        \   'modified': 'MyModified',
        \ }
  let g:lightline.tabline = {'left': [['tabs']], 'right': []}
  " let g:lightline.tabline = {'left': [['buffers']], 'right': []}
  let g:lightline.tab = {
        \ 'active': [ 'tabnum', 'filetype', 'mytabname', 'modified' ],
        \ 'inactive': [ 'tabnum', 'filetype', 'mytabname', 'modified' ] }
  set guioptions-=e
  " Display tab name, but use directory name if an index.js file is present:
  " Sourced from airline-vim
  function! MyTabName(n)
    let buflist = tabpagebuflist(a:n)
    let winnr = tabpagewinnr(a:n)
    let buf = expand('#'.buflist[winnr - 1])
    let filename = fnamemodify(buf, ":t")
    " expand('#'.buflist[winnr - 1].':t') " buflist[winnr - 1]
    if filename == 'index.js' || filename == 'index.jsx' || filename == 'index.ts' || filename == 'index.tsx'
      return fnamemodify(buf, ':p:h:t') . '/i'
    endif
    return filename
  endfunction
  " ✎
  function! MyModified(n)
    let winnr = tabpagewinnr(a:n)
    return gettabwinvar(a:n, winnr, '&modified') ? '✎' : gettabwinvar(a:n, winnr, '&modifiable') ? '' : ''
  endfunction
  " Display current filetype in tab name:
  function! MyTabFiletype(n)
    let buflist = tabpagebuflist(a:n)
    let winnr = tabpagewinnr(a:n)
    let buf = expand('#'.buflist[winnr - 1])
    return winwidth(0) > 70 ? (strlen(&filetype) ? WebDevIconsGetFileTypeSymbol(buf) : '') : ''
  endfunction

  " Display Current Filetype in Status Bar
  function! MyFiletype()
    return winwidth(0) > 70 ? (strlen(&filetype) ? WebDevIconsGetFileTypeSymbol() : '') : ''
  endfunction

  " WordCount
  let g:word_count="<unknown>"
  fun! WordCount()
      return g:word_count
  endfun
  fun! UpdateWordCount()
      let s = system("wc -w ".expand("%p"))
      let parts = split(s, ' ')
      if len(parts) > 1
          let g:word_count = parts[0]
      endif
  endfun
  augroup WordCounter
      au! CursorHold * call UpdateWordCount()
      au! CursorHoldI * call UpdateWordCount()
  augroup END

  function! MyGitGutter()
    if ! exists('*GitGutterGetHunkSummary')
          \ || ! get(g:, 'gitgutter_enabled', 0)
          \ || winwidth('.') <= 90
      return ''
    endif
    let symbols = [
          \ g:gitgutter_sign_added,
          \ g:gitgutter_sign_modified,
          \ g:gitgutter_sign_removed,
          \ ]
    let hunks = GitGutterGetHunkSummary()
    let ret = []
    for i in [0, 1, 2]
      if hunks[i] > 0
        call add(ret, symbols[i] . hunks[i])
      endif
    endfor
    return join(ret, ' ')
  endfunction
"}}
"Lightline Buffer {{
  " let g:lightline#bufferline#enable_devicons = 1
  " let g:lightline#bufferline#unicode_symbols = 1
  " let g:lightline#bufferline#filename_modifier = ':s?index\.js?i?:.'
  " let g:lightline#bufferline#show_number = 2

  " Buffer controls:
  " nmap <Leader>1 <Plug>lightline#bufferline#go(1)
  " nmap <Leader>2 <Plug>lightline#bufferline#go(2)
  " nmap <Leader>3 <Plug>lightline#bufferline#go(3)
  " nmap <Leader>4 <Plug>lightline#bufferline#go(4)
  " nmap <Leader>5 <Plug>lightline#bufferline#go(5)
  " nmap <Leader>6 <Plug>lightline#bufferline#go(6)
  " nmap <Leader>7 <Plug>lightline#bufferline#go(7)
  " nmap <Leader>8 <Plug>lightline#bufferline#go(8)
  " nmap <Leader>9 <Plug>lightline#bufferline#go(9)
  " nmap <Leader>0 <Plug>lightline#bufferline#go(10)
"}}

