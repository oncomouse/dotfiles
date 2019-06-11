" Autocomplete:
" Deoplete {{
  "inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
  "let g:deoplete#enable_at_startup = 1
  "" call deoplete#enable()
  "augroup DeopleteEnable
    "au! VimEnter * nested call deoplete#initialize()
  "augroup end
""}}
" Deoplete-TernJS {{
  "let g:deoplete#sources#ternjs#tern_bin = '/usr/local/bin/tern'
  "let g:deoplete#sources#ternjs#timeout = 1
  "let g:deoplete#sources#ternjs#filetypes = [
      "\ 'jsx',
      "\ 'javascript.jsx',
      "\ 'vue',
      "\ ]
"}}
