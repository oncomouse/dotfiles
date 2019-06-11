" Linter:
" ALE {{
  " Better ALE Msg Format
  let g:ale_echo_msg_error_str = 'E'
  let g:ale_echo_msg_warning_str = 'W'
  let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
  " Jump between ALE Errors:
  nmap <silent> <C-k> <Plug>(ale_previous_wrap)
  nmap <silent> <C-j> <Plug>(ale_next_wrap)
  " Lint only on save:
  " let g:ale_lint_on_text_changed = 'never'
  " let g:ale_lint_on_enter = 1
"}}

