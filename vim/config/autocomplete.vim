" Autocomplete:
" Deoplete {{{
  call deoplete#custom#var('omni', 'input_patterns', {
  \ 'pandoc': '@[\w\-_\:\.]+',
  \ 'clojure': '[\w!$%&*+/:<=>?@\^_~\-\.#]*',
  \ 'go': '[^. *\t]\.\w*',
  \})
" }}}
" LanguageClient {{{
  let g:LanguageClient_serverCommands = {
      \ 'javascript': ['typescript-language-server', '--stdio'],
      \ 'reason': ['reason-language-server'],
      \ 'html': ['html-languageserver', '--stdio'],
      \ 'json': ['json-languageserver', '--stdio'],
      \ 'css': ['css-languageserver', '--stdio'],
      \ 'scss': ['css-languageserver', '--stdio'],
      \}
  let g:LanguageClient_settingsPath = expand('~/dotfiles/vim/lsp.settings.json')
  let g:LanguageClient_diagnosticsEnable=0
  let g:LanguageClient_useVirtualText=1
  " let g:LanguageClient_loggingLevel='DEBUG'
  " let g:LanguageClient_serverStderr=expand('~/lsp-server.log')
  " let g:LanguageClient_loggingFile = expand('~/lsp-client.log')
  " Set our default mappings with a function:
  call dotfiles#lsp#mappings()
 
  " Formatting:
  vmap <silent> <leader>f :call LanguageClient#textDocument_rangeFormatting()
  nmap <silent> <leader>f :call LanguageClient#textDocument_rangeFormatting()
  command! -nargs=0 Format :call LanguageClient#textDocument_formatting()
  " Autoformat on save:
  set formatexpr=LanguageClient#textDocument_rangeFormatting_sync()
  augroup lsp-format-on-save
    autocmd!
    autocmd BufWritePre * :call LanguageClient#textDocument_formatting_sync()
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
    autocmd FileType go :autocmd! BufEnter <buffer> :call <SID>goKeyMappings(1)
    autocmd FileType go :autocmd! BufLeave <buffer> :call <SID>goKeyMappings(0)
  augroup END

  function! s:goKeyMappings(...) abort
    let l:on = get(a:, 0, 0)
    if l:on
      nmap  <silent> gd :GoDef<CR>
      nmap <silent> gy :GoDefType<CR>
      nmap <silent> gi :GoImplements<CR>
      nmap <silent> gr :GoReferrers<CR>
      nmap <silent> U :GoDocBrowser<CR>
      nmap <silent> <leader>rn :GoRename<CR>
      nmap <leader>rt <Plug>(go-run-tab)
      nmap <leader>rs <Plug>(go-run-split)
      nmap <leader>rv <Plug>(go-run-vertical)
    else
      call dotfiles#lsp#mappings()
    endif
  endfunction
" }}}
" # vim:foldmethod=marker
