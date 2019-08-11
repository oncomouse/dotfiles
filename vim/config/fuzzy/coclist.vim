" CoC-List {{{
  if &runtimepath =~# 'coc'
    call coc#config('list.source.grep.command', 'ag')
    command! -nargs=+ -complete=custom,s:GrepArgs Ag exe 'CocList grep '.<q-args>

    function! s:GrepArgs(...)
      let list = ['-S', '-smartcase', '-i', '-ignorecase', '-w', '-word',
            \ '-e', '-regex', '-u', '-skip-vcs-ignores', '-t', '-extension']
      return join(list, "\n")
    endfunction
    nnoremap <silent> <c-p> :exe 'CocList files'<CR>
    nnoremap <silent> <leader>a :exe 'CocList buffers'<CR>
    nnoremap <silent> <leader>A :exe 'CocList windows'<CR>
    nnoremap <silent> <leader>l :exe 'CocList lines'<CR>
    nnoremap <silent> <leader>? :exe 'CocList cmdhistory'<CR>
    nnoremap <silent> <leader>rr  :<C-u>CocList -A --normal yank<cr>
    " Close the quick fix window:
    nnoremap <silent> <leader>cc :cwindow<CR>
    nnoremap <silent> <leader>ll :lwindow<CR>
    nnoremap <silent> <leader>/ :execute 'Ag ' . input('Ag/')<CR>
    nnoremap <silent> K :call dotfiles#ag#SearchWordWithAg()<CR>
    vnoremap <silent> K :call dotfiles#ag#SearchVisualSelectionWithAg()<CR>
  endif 
" }}}

" # vim:foldmethod=marker
