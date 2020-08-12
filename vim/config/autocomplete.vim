scriptencoding utf-8
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
" Denite {{{
  " \ 'mode' : 'insert',
  " \ 'start_filter' : 1,
  " \ 'quit' : 1,
  " \ 'highlight_matched_char' : 'MoreMsg',
  " \ 'highlight_matched_range' : 'MoreMsg',
  call denite#custom#option('default', 'winheight', 10)
  call denite#custom#option('default', 'direction', 'rightbelow')
  call denite#custom#option('default', 'statusline', v:false)
  call denite#custom#var('file/rec', 'command', ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])
  call denite#custom#option('default', 'prompt', 'λ')
  call denite#custom#var('grep', 'command', ['ag'])
  call denite#custom#var('grep', 'default_opts', ['-i', '--vimgrep'])
  call denite#custom#var('grep', 'recursive_opts', [])
  call denite#custom#var('grep', 'pattern_opt', [])
  call denite#custom#var('grep', 'separator', ['--'])
  call denite#custom#var('grep', 'final_opts', [])
  call denite#custom#source('file_rec', 'sorters', ['sorter/sublime'])
  call denite#custom#filter('matcher_ignore_globs', 'ignore_globs',
        \ [ '.git/', '.ropeproject/', '__pycache__/*', '*.pyc', 'node_modules/',
        \   'venv/', 'images/', '*.min.*', 'img/', 'fonts/', '*.png'])
  command!      -bang -nargs=? -complete=dir FZF    exe 'Denite -auto-action=preview -floating-preview -match-highlight -split=floating -vertical-preview -start-filter file/rec '.<q-args>
  " Open location list:
  nmap <silent> <leader>d :<C-u>Denite -split=floating -vertical-preview -auto-action=preview -floating-preview location_list<CR>
  " Old FZF Interface:
  command! Buffers :exe 'Denite -split=floating buffer'
  command! Windows :exe 'Denite window'
  command! BLines :exe 'Denite line'
  command! History :exe 'Denite command_history'
  nnoremap <silent> <leader>y  :<C-u>Denite -split=floating neoyank<CR>
  " Implement Ag
  command! -nargs=+ -complete=custom,s:GrepArgs Ag exe 'Denite grep:::'.<q-args>
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

  " Only load LSP commands if we are in a buffer where they exist:
  function! LC_maps() abort
    if has_key(g:LanguageClient_serverCommands, &filetype)
      nmap <F5> <Plug>(lcn-menu)
      " Or map each action separately
      nmap <silent> gd <Plug>(lcn-definition)
      nmap <silent> gy <Plug>(lcn-type-definition)
      nmap <silent> gi <Plug>(lcn-implementation)
      nmap <silent> gr :<C-u>Denite references
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
