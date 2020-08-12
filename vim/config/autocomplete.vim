scriptencoding utf-8
set completeopt-=preview
" ALE {{{
  let g:ale_javascript_standard_executable = 'semistandard'
  command! Format ALEFix
  let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
  "
  " Jump between ALE Errors:
  nmap <silent> [d :<C-u>ALEPreviousWrap<CR>
  nmap <silent> ]d :<C-u>ALENextWrap<CR>
  "
  let g:ale_lint_on_insert_leave = 1
  let g:ale_cursor_detail = 0
  let g:ale_disable_lsp = 1
  let g:ale_fix_on_save = 1
  let g:ale_pattern_options = {
    \  '\.min.js$': {'ale_enabled': 0},
    \  'build/.*$': {'ale_enabled': 0},
    \}
" }}}
" vim-clap {{{
  " Disable location list:
  nmap <silent> <leader>d :<C-u>Clap loclist<CR>
  " Old FZF Interface:
  command!      -bang -nargs=? -complete=dir FZF    exe 'Clap files ++query='.<q-args>
  command! Buffers :exe 'Clap buffers'
  command! Windows :exe 'Clap windows'
  command! BLines :exe 'Clap lines'
  command! History :exe 'Clap command_history'
  nnoremap <silent> <leader>y  :<C-u>Clap yanks<CR>
  " Implement Ag
  command! -nargs=+ -complete=custom,s:GrepArgs Ag exe 'Clap grep2 ++query='.<q-args>
  vnoremap <silent> <leader>/ :<C-u>Clap grep2 ++query=@visual<CR>
" }}}
" Deoplete {{{
let g:deoplete#enable_at_startup = 1
" }}}
" LanguageClient-neovim {{{
  " Turn off all diagnostic stuff (pump it all to ALE):
  let g:LanguageClient_diagnosticsEnable = v:false
  " Always use hover:
  let g:LanguageClient_hoverPreview = 'Always'
  " Documentation Function
  function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
      execute 'h '.expand('<cword>')
    else
      call LanguageClient#textDocument_hover()
    endif
  endfunction

  " Use Clap for selection in LSP:
  function! MySelectionUI(source, sink) abort
    return clap#run({'id': 'LCN', 'source': a:source, 'sink': a:sink})
  endfunction
  let g:LanguageClient_selectionUI = function('MySelectionUI')

  " Only load LSP commands if we are in a buffer where they exist:
  function! LC_maps() abort
    if has_key(g:LanguageClient_serverCommands, &filetype)
      nmap <F5> <Plug>(lcn-menu)
      " Or map each action separately
      nmap <silent> gd <Plug>(lcn-definition)
      nmap <silent> gy <Plug>(lcn-type-definition)
      nmap <silent> gi <Plug>(lcn-implementation)
      nmap <silent> gr <Plug>(lnc-references)
      nmap <silent> K :<C-u>call <SID>show_documentation()<CR>
    endif
  endfunction
  augroup lc_maps
    autocmd FileType * call LC_maps()
  augroup END
  " Turn off diagnostics in solargraph and just run rubocop through ALE:
  let g:LanguageClient_serverCommands = {
        \ 'javascript': ['/usr/local/bin/typescript-language-server', '--stdio'],
        \ 'javascriptreact': ['/usr/local/bin/typescript-language-server', '--stdio'],
        \ 'typescript': ['/usr/local/bin/typescript-language-server', '--stdio'],
        \ 'typescriptreact': ['/usr/local/bin/typescript-language-server', '--stdio'],
        \ 'ruby': ['~/.asdf/shims/solargraph', 'stdio'],
        \ 'html': ['/usr/local/bin/html-languageserver', '--stdio'],
        \ 'scss': ['/usr/local/bin/css-languageserver', '--stdio'],
        \ 'css': ['/usr/local/bin/css-languageserver', '--stdio'],
        \ 'json': ['/usr/local/bin/json-languageserver', '--stdio'],
        \ 'vim': ['/usr/local/bin/vim-language-server', '--stdio'],
        \}
  " }}}
" # vim:foldmethod=marker
