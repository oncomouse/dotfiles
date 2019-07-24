" Autocomplete:
" Deoplete {{{
  call deoplete#custom#var('omni', 'input_patterns', {
  \ 'pandoc': '@[\w\-_\:\.]+',
  \ 'clojure': '[\w!$%&*+/:<=>?@\^_~\-\.#]*',
  \})
" }}}
" LanguageClient {{{
  let g:LanguageClient_serverCommands = {
      \ 'reason': ['reason_language_server'],
      \ 'go': ['gopls'],
      \ 'html': ['html-languageserver', '--stdio'],
      \ 'json': ['json-languageserver', '--stdio'],
      \ 'css': ['css-languageserver', '--stdio'],
      \ 'scss': ['css-languageserver', '--stdio'],
      \ 'javascript': ['javascript-typescript-languageserver'],
      \ 'javascript.jsx': ['javascript-typescript-languageserver'],
      \ 'typescript': ['javascript-typescript-languageserver'],
      \}
  let g:LanguageClient_settingsPath = expand('~/dotfiles/vim/lsp.settings.json')
  let g:LanguageClient_diagnosticsEnable=0

  " Remap keys for gotos
  nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
  " Use U to show documentation in preview window
  nnoremap <silent> U :call LanguageClient#textDocument_hover()<CR>
  " Remap for rename current word
  nmap <leader>rn :call LanguageClient#textDocument_rename()<CR>

  " Disable a bunch of vim-go features that ALE does:
  let g:go_def_mapping_enabled = 0
  let g:go_def_mode='gopls'
  let g:go_info_mode='gopls'
  let g:go_metalinter_enabled = []
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
" # vim:foldmethod=marker
