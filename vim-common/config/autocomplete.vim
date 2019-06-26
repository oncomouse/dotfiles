" Autocomplete:
" COC {{
  " Disable CoC for CSS:
  " augroup disable-coc-for-css
  "   autocmd!
  "   autocmd FileType css,scss let b:coc_suggest_disable = 1
  " augroup END
  " Add extensions
  if !isdirectory(expand('~/.config/coc/extensions/node_modules/coc-gocode'))
    call coc#add_extension(
          \'coc-omni',
          \'coc-neosnippet',
          \'coc-tsserver',
          \'coc-json',
          \'coc-html',
          \'coc-gocode',
          \'coc-css',
          \)
  endif
  call coc#config('suggest.snippetIndicator', '►')
  " Configure omnifunc completion for Pandoc:
  call coc#config('coc.source.omni.filetypes',
        \ [
        \   'pandoc'
        \ ]
        \)
  " JavaScript Config:
  call coc#config('javascript.format', {
      \   'placeOpenBraceOnNewLineForFunctions': 1,
      \   'placeOpenBraceOnNewLineForControlBlocks': 1,
      \   'insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces': 1,
      \})

  " Use clojure-lsp for clojure completion:
  call coc#config('languageserver', {
  \    'clojure-lsp': {
  \      'command': 'bash',
  \      'args': ['-c', 'clojure-lsp'],
  \      'filetypes': ['clojure'],
  \      'rootPatterns': ['project.clj'],
  \      'additionalSchemes': ['jar', 'zipfile'],
  \      'trace.server': 'verbose',
  \      'initializationOptions': {
  \      }
  \    }
  \  }
  \)
" }}
"" Neosnippets {{
  "
  let g:neosnippet#disable_runtime_snippets = {
        \ 'css': 1,
        \ 'pandoc': 1,
        \ 'markdown': 1,
        \}

  " call coc#config('suggest.noselect', 0)
  " use <tab> for trigger completion and navigate to the next complete item
  function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
  endfunction

  inoremap <silent><expr> <S-Tab>
        \ pumvisible() ? "\<C-p>" :
        \ <SID>check_back_space() ? "\<S-Tab>" :
        \ coc#refresh()
  inoremap <silent><expr> <TAB>
  \ pumvisible() ? coc#_select_confirm() :
  \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ coc#refresh()
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
