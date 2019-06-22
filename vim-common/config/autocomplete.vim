" Autocomplete:
" Deoplete {{
  function! s:check_back_space() abort "{{{
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
  endfunction"}}}
  " inoremap <silent><expr> <TAB>
  "       \ pumvisible() ? "\<C-n>" :
  "       \ <SID>check_back_space() ? "\<TAB>" :
  "       \ deoplete#manual_complete()
  imap <expr><TAB>
  \ pumvisible() ? "\<C-n>" :
  \ neosnippet#expandable_or_jumpable() ?
  \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
  smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
  \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
"}}
" Neosnippets {{
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
