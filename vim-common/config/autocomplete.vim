" Autocomplete:
" YCM {{
  " Use deoplete for Pandoc:
  let g:ycm_filetype_blacklist = { 'pandoc': 1 }
" }}
" Deoplete {{
  "
  "function! s:check_back_space() abort "{{{
  "  let col = col('.') - 1
  "  return !col || getline('.')[col - 1]  =~ '\s'
  "endfunction"}}}
  ""
  "" Shift-tab to go up the list:
  "imap <expr><S-TAB>
  "      \ pumvisible() ? "\<C-p>" : "<\S-TAB>"
  "" Tab to go down the list:
  "imap <expr><TAB>
  "\ pumvisible() ?
  "\ "\<C-n>" :
  "\ neosnippet#expandable_or_jumpable() ?
  "\    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
  "smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
  "\ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
  "inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")
""}}
"" Neosnippets {{
  "imap <C-k>     <Plug>(neosnippet_expand_or_jump)
  "smap <C-k>     <Plug>(neosnippet_expand_or_jump)
  "xmap <C-k>     <Plug>(neosnippet_expand_target)
  ""
  "" Enable snipMate compatibility feature.
  "let g:neosnippet#snippets_directory = expand('~/dotfiles/vim-common/snippets')
  "let g:neosnippet#enable_snipmate_compatibility = 1
  "augroup snippets
  "  autocmd!
  "  autocmd FileType neosnippet setlocal tabstop=8 shiftwidth=8 softtabstop=8 expandtab=noexpandtab
  "augroup END
" }}
" Deoplete-TernJS {{
  " let g:deoplete#sources#ternjs#tern_bin = '/usr/local/bin/tern'
  " let g:deoplete#sources#ternjs#timeout = 1
  " let g:deoplete#sources#ternjs#docs = 1
  " let g:deoplete#sources#ternjs#filetypes = [
  "     \ 'javascript.jsx',
  "     \ ]
"}}
