function! dotfiles#autocomplete#ncm2#init() abort
  set completeopt=noinsert,menuone,noselect
  augroup ncm2-start
    autocmd!
    autocmd BufEnter * call ncm2#enable_for_buffer()
    au User Ncm2Plugin call ncm2#register_source({
        \ 'name' : 'fish',
        \ 'priority': 9,
        \ 'subscope_enable': 1,
        \ 'scope': ['fish'],
        \ 'mark': 'fish',
        \ 'word_pattern': '[\w\-]+',
        \ 'complete_pattern': ':\s*',
        \ 'on_complete': ['ncm2#on_complete#omni', 'fish#Complete'],
        \ })
    au User Ncm2Plugin call ncm2#register_source({
        \ 'name' : 'lua',
        \ 'priority': 9,
        \ 'subscope_enable': 1,
        \ 'scope': ['lua'],
        \ 'mark': 'lua',
        \ 'word_pattern': '\w+|[^. *\t][.:]\w*',
        \ 'complete_pattern': ':\s*',
        \ 'on_complete': ['ncm2#on_complete#omni', 'xolox#lua#completefunc'],
        \ })
  augroup END
endfunction

function! dotfiles#autocomplete#ncm2#writing() abort
  let g:ncm2_biblatex#bibfile = g:bibliography_file
  let g:ncm2_biblatex#addinfo = v:true
  let g:ncm2_biblatex#reloadbibfileonchange = v:true
  augroup biblatex_markdown
    autocmd!
    autocmd FileType markdown let b:ncm2_biblatex_enabled = v:true
    autocmd FileType markdown call ncm2#blacklist_for_buffer(['buf', 'tmux'])
  augroup END
endfunction
