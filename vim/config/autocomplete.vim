" Autocomplete:
" Deoplete {{{
  call deoplete#custom#var('omni', 'input_patterns', {
  \ 'pandoc': '@[\w\-_\:\.]+',
  \ 'clojure': '[\w!$%&*+/:<=>?@\^_~\-\.#]*',
  \ 'go': '[^. *\t]\.\w*',
  \})
" }}}
" LanguageClient {{{
  " Only set up LSP if it is in the runtime path:
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
  let g:lsp_started = {}
  function! s:start_lsp(server_executable, ...) abort
    if has_key(g:lsp_started, a:server_executable)
      return
    endif
    let g:lsp_started[a:server_executable] = 1
    call jobstart ( a:server_executable, { 'detach' : 1 } )
  endfunction
  augroup lsp-load-settings
    autocmd!
    autocmd BufEnter * if dotfiles#lsp_test() | call dotfiles#lsp#load() | endif
    " autocmd BufReadPre *.rb call <SID>start_lsp('solargraph socket')
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
