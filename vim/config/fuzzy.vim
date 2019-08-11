" FZF Colors {{{
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'Visual', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'MoreMessage'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Special'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] } 
" }}}
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
  endif 
" }}}
" FZF {{{
  " Highlight file with <shift>-<tab>; press the follow to open:
  if &runtimepath =~# 'fzf'
    let g:fzf_action = {
        \ 'ctrl-s': 'split',
        \ 'ctrl-v': 'vsplit',
        \ 'ctrl-t': 'tabnew',
        \ 'ctrl-e': 'edit',
        \ }
    nnoremap <silent> <c-p> :FZF<CR>
    nnoremap <silent> <leader>F :FZF ~<CR>
    let g:fzf_nvim_statusline = 0 " disable statusline overwriting

    " Close the quick fix window:
    nnoremap <silent> <leader>cc :cwindow<CR>
    nnoremap <silent> <leader>ll :lwindow<CR>

    nnoremap <silent> <leader>a :Buffers<CR>
    nnoremap <silent> <leader>A :Windows<CR>
    nnoremap <silent> <leader>l :BLines<CR>
    nnoremap <silent> <leader>? :History<CR>
    nnoremap <silent> <leader>/ :execute 'Ag ' . input('Ag/')<CR>
    nnoremap <silent> K :call dotfiles#ag#SearchWordWithAg()<CR>
    vnoremap <silent> K :call dotfiles#ag#SearchVisualSelectionWithAg()<CR>
    
    " Access yank history:
    " let g:neoyank#file = $HOME.'/.vim/yankring.txt'
    " nnoremap <leader>rr :FZFNeoyank *<cr>
    " nnoremap <leader>RR :FZFNeoyank * P<cr>
    " vnoremap <leader>rr :FZFNeoyankSelection<cr>
    " Complete file name:
    imap <C-x><C-f> <plug>(fzf-complete-file-ag)
    " Complete file line:
    imap <C-x><C-l> <plug>(fzf-complete-line)
  endif
  "
" }}}

" # vim:foldmethod=marker
