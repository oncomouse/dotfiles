" Autocomplete:
" Deoplete {{{
  let g:deoplete#enable_at_startup = 1
  call deoplete#custom#source('file', 'rank', 1000)
  let g:AutoPairsMapCR=0
  let g:deoplete#auto_complete_start_length = 1
  let g:deoplete#enable_at_startup = 1
  let g:deoplete#enable_smart_case = 1
  call deoplete#custom#var('omni', 'input_patterns', {
  \ 'pandoc': '@[\w\-_\:\.]+',
  \ 'clojure': '[\w!$%&*+/:<=>?@\^_~\-\.#]*',
  \ 'go': '[^. *\t]\.\w*',
  \})
  function! SmartTab() abort
    let l:emmetTypes = ['css', 'elm', 'haml', 'html', 'jade', 'less', 'sass', 'scss', 'slim']
    if index(l:emmetTypes, &filetype) >= 0
      return emmet#expandAbbrIntelligent("\<tab>")
    else
      return "\<tab>"
    endif
  endfunction
  imap <expr><TAB> pumvisible() ? "\<C-n>" : SmartTab()
  imap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
  " exec 'imap <expr><CR> pumvisible() ? deoplete#close_popup() : "\<CR>\<Plug>DiscretionaryEnd\<Plug>AutoPairsReturn"'
  " imap <expr><Esc> pumvisible() ? deoplete#cancel_popup() : "\<ESC>"
" }}}
" LanguageClient {{{
  " Basic settings
  let g:LanguageClient_serverCommands = {
      \ 'javascript': ['typescript-language-server', '--stdio'],
      \ 'javascript.jsx': ['typescript-language-server', '--stdio'],
      \ 'reason': ['reason-language-server'],
      \ 'html': ['html-languageserver', '--stdio'],
      \ 'json': ['json-languageserver', '--stdio'],
      \ 'css': ['css-languageserver', '--stdio'],
      \ 'scss': ['css-languageserver', '--stdio'],
      \ 'ruby': ['solargraph', 'stdio'],
      \}
  let g:LanguageClient_settingsPath = expand('~/dotfiles/vim/lsp.settings.json')
  let g:LanguageClient_diagnosticsEnable=0
  let g:LanguageClient_useVirtualText=1
  " let g:LanguageClient_loggingLevel='DEBUG'
  " let g:LanguageClient_serverStderr=expand('~/lsp-server.log')
  " let g:LanguageClient_loggingFile = expand('~/lsp-client.log')
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
