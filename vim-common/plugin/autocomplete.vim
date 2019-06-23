" Autocomplete:
" COC {{
  " Disable CoC for CSS:
  augroup disable-coc-for-css
    autocmd!
    autocmd FileType css,scss let b:coc_suggest_disable = 1
  augroup END
  " Add extensions
  if !isdirectory(expand('~/.config/coc/extensions/node_modules/coc-gocode'))
    call coc#add_extension(
          \'coc-omni',
          \'coc-neosnippet',
          \'coc-tsserver',
          \'coc-json',
          \'coc-html',
          \'coc-gocode',
          \)
  endif
  call coc#config('suggest.snippetIndicator', '►')
  " Configure omnifunc completion for Clojure:
  call coc#config('coc.source.omni.filetypes',
        \ [
        \   'clojure',
        \   'pandoc'
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
  let g:neosnippet#disable_runtime_snippets = {
        \ 'css': 1,
        \ 'pandoc': 1,
        \ 'markdown': 1,
        \}

  call coc#config('suggest.noselect', 0)
  "
  imap <C-k>     <Plug>(neosnippet_expand_or_jump)
  smap <C-k>     <Plug>(neosnippet_expand_or_jump)
  xmap <C-k>     <Plug>(neosnippet_expand_target)
  let g:coc_snippet_next = '<C-k>'
  let g:coc_snippet_prev = '<C-j>'
  "
  " Enable snipMate compatibility feature.
  let g:neosnippet#snippets_directory = expand('~/dotfiles/vim-common/snippets')
  let g:neosnippet#enable_snipmate_compatibility = 1
  augroup snippets
    autocmd!
    autocmd FileType neosnippet setlocal tabstop=8 shiftwidth=8 softtabstop=8 expandtab=noexpandtab
  augroup END
" }}
