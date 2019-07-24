" Autocomplete:
" COC {{
  " Add extensions
  let coc_extensions = [
  \   'coc-omni',
  \   'coc-neosnippet',
  \   'coc-tsserver',
  \   'coc-json',
  \   'coc-html',
  \   'coc-css',
  \   'coc-python',
  \]
  function! s:load_extension(ext) abort
    if !isdirectory(expand('~/.config/coc/extensions/node_modules/'.a:ext))
      call coc#add_extension(a:ext)
    endif
  endfunction
  for ext in coc_extensions
    call <SID>load_extension(ext)
  endfor
  " call coc#config('tsserver', {
  "     \  'log': 'verbose',
  "     \  'trace.server': 'verbose',
  "     \ })
  if executable('reason-language-server')
    call coc#config('languageserver.reason', {
      \  'command': 'reason-language-server',
      \  'filetypes': ['reason'],
      \  'trace.server': 'verbose',
      \  'rootPatterns': ['bsconfig.json', 'package.json', '.git/', '.merlin'],
      \  'settings': {'reason_language_server' : {'format_width': 80}},
      \})
  endif
  " Use <c-space> to trigger completion.
  inoremap <silent><expr> <c-space> coc#refresh()

  " Remap keys for gotos
  nmap <silent> gd <Plug>(coc-definition)
  nmap <silent> gy <Plug>(coc-type-definition)
  nmap <silent> gi <Plug>(coc-implementation)
  nmap <silent> gr <Plug>(coc-references)

  " Use U to show documentation in preview window
  nnoremap <silent> U :call <SID>show_documentation()<CR>

  " Remap for rename current word
  nmap <leader>rn <Plug>(coc-rename)

  " Disable a bunch of vim-go features that ALE does:
  let g:go_def_mapping_enabled = 0
  let g:go_def_mode='gopls'
  let g:go_info_mode='gopls'
  let g:go_metalinter_enabled = []
  call coc#config('languageserver.golang',{
        \  "command": "gopls",
        \  "rootPatterns": ["go.mod", ".vim/", ".git/", ".hg/"],
        \  "filetypes": ["go"],
        \})
  " Disable linters we use in CoC:
  let g:ale_linters = {
    \  'reason': [
    \    'merlin',
    \  ],
    \ 'javascript': [
    \    'eslint',
    \    'flow',
    \    'jshint',
    \  ],
    \}
  call coc#config('suggest.snippetIndicator', '►')
  " Configure omnifunc completion for Pandoc:
  call coc#config('coc.source.omni.filetypes', [
      \   'pandoc',
      \])
  " JavaScript Config:
  " Don't do typechecking for JavaScript:
  call coc#config('javascript.validate.enable', 0)
  " Format JavaScript the way I like:
  call coc#config('javascript.format', {
      \   'insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces': 0,
      \   'insertSpaceBeforeFunctionParenthesis': 0,
      \})
  " Format on save:
  call coc#config('coc.preferences.formatOnSaveFiletypes', [
      \ 'javascript',
      \ 'javascript.jsx',
      \ 'go',
      \ 'reason',
      \ 'python',
      \])
  " CoC Formatting:
  vmap <silent> <leader>f <Plug>(coc-format-selected)<CR>
  nmap <silent> <leader>f <Plug>(coc-format-selected)<CR>
  command! -nargs=0 Format :call CocAction('format')
  " call coc#config('suggest.noselect', 0)
  " Use tab for trigger completion with characters ahead and navigate.
  " Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.

  function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
  endfunction
  inoremap <silent><expr> <TAB>
        \ pumvisible() ? "\<Down>" :
        \ <SID>check_back_space() ? "\<TAB>" :
        \ coc#refresh()
  " }}
