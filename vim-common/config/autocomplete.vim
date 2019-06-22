" Autocomplete:
" COC {{
  " Configure omnifunc completion for Clojure:
  call coc#config('coc.source.omni.filetypes',
        \ [
        \   'clojure'
        \ ]
        \)
  " Use vim-clojure-static for the omnifunc:
  augroup clojure-autocomplete
    autocmd!
    autocmd BufEnter,WinEnter * if &filetype=='clojure'|setlocal omnifunc=clojurecomplete#Complete|endif
  augroup END
" }}
"" Neosnippets {{
  "
  inoremap <silent><expr> <TAB>
      \ pumvisible() ? coc#_select_confirm() :
      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

  function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
  endfunction
  let g:coc_snippet_next = '<tab>'
  call coc#config('suggest.noselect', 0)
  "
  imap <C-k>     <Plug>(neosnippet_expand_or_jump)
  smap <C-k>     <Plug>(neosnippet_expand_or_jump)
  xmap <C-k>     <Plug>(neosnippet_expand_target)
  "
  " Enable snipMate compatibility feature.
  let g:neosnippet#snippets_directory = expand('~/dotfiles/vim-common/snippets')
  let g:neosnippet#enable_snipmate_compatibility = 1
  augroup snippets
    autocmd!
    autocmd FileType neosnippet setlocal tabstop=8 shiftwidth=8 softtabstop=8 expandtab=noexpandtab
  augroup END
" }}
