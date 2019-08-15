" Autocomplete:
" CoC {{{
  " Add extensions
  let g:coc_global_extensions = [
  \   'coc-css',
  \   'coc-fish',
  \   'coc-html',
  \   'coc-json',
  \   'coc-lists',
  \   'coc-omni',
  \   'coc-pairs',
  \   'coc-python',
  \   'coc-solargraph',
  \   'coc-tsserver',
  \   'coc-vimlsp',
  \   'coc-yank',
  \]
  " call coc#config('tsserver', {
  "     \  'log': 'verbose',
  "     \  'trace.server': 'verbose',
  "     \ })
  " if executable('reason-language-server')
  "   call coc#config('languageserver.reason', {
  "     \  'command': 'reason-language-server',
  "     \  'filetypes': ['reason'],
  "     \  'trace.server': 'verbose',
  "     \  'rootPatterns': ['bsconfig.json', 'package.json', '.git/', '.merlin'],
  "     \  'settings': {'reason_language_server' : {'format_width': 80}},
  "     \})
  " endif
  function! SmartTab() abort
    let l:emmetTypes = ['css', 'elm', 'haml', 'html', 'jade', 'less', 'sass', 'scss', 'slim']
    if index(l:emmetTypes, &filetype) >= 0
      return emmet#expandAbbrIntelligent("\<tab>")
    else
      return "\<tab>"
    endif
  endfunction
  " Load errors in ALE:
  call coc#config('coc.preferences.diagnostic.displayByAle', 1)
  imap <expr><TAB> pumvisible() ? "\<C-n>" : SmartTab()
  imap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
  augroup lsp-load-settings
    autocmd!
    autocmd BufEnter * if dotfiles#lsp_test() | call dotfiles#lsp#load() | endif
    " Since neither ale nor neovim-languageclient proivde a mechanism to start
    " a socket-based LSP, this function starts one based on file type. Also,
    " this can be used to run one LSP and share it between Ale and deoplete,
    " but that doesn't really work out too often in practice:
    " autocmd BufReadPre *.rb call dotfiles#lsp#start_lsp('solargraph socket')
  augroup END
" }}}
" Vim-Go Support {{{
  " B/c vim-go is a full-featured IDE on it's own, we have to set it up to work
  " with deoplete + LSP (by turning off LSP and turning on Vim-Go's stuff).
  " Also, see above, where the omnifunc is set for go (which uses gopls).
  let g:go_def_mapping_enabled = 0
  let g:go_def_mode='gopls'
  let g:go_info_mode='gopls'
  let g:go_metalinter_enabled = [] " Turn off metalinter in favor of ALE.
  augroup go-vim-definitions
    autocmd!
    autocmd FileType go :autocmd! BufEnter <buffer> :call dotfiles#go#mappings(1)
    autocmd FileType go :autocmd! BufLeave <buffer> :call dotfiles#go#mappings(0)
  augroup END
" }}}
" # vim:foldmethod=marker
