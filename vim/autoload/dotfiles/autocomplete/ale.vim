function! dotfiles#autocomplete#ale#init() abort
  let g:ale_javascript_standard_executable = 'semistandard'
  " Check if we turned off ALE formatting in favor of LSP formatting:
  command! Format exe ':ALEFix'
  let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
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
endfunction
