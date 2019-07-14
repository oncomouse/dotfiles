" Ale {{
  " Better ALE Msg Format
  " let g:ale_echo_msg_error_str = 'E'
  " let g:ale_echo_msg_warning_str = 'W'
  let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
  "
  " Jump between ALE Errors:
  nmap <silent> <leader>h <Plug>(ale_previous_wrap)
  nmap <silent> <leader>j <Plug>(ale_next_wrap)
  nmap <silent> <leader>d :ALEDetail<CR>
  "
  " Lint only on save:
  " let g:ale_lint_on_text_changed = 'never'
  let g:ale_lint_on_enter = 1
  "
  " Use ALE w/ CoC:
  call coc#config('coc.preferences.diagnostic.displayByAle', 1)
  "
  " Add Vale to pandoc bc vim-pandoc insists on changing filetype:
  call ale#linter#Define('pandoc', {
  \   'name': 'vale',
  \   'executable': 'vale',
  \   'command': 'vale --output=JSON %t',
  \   'callback': 'ale#handlers#vale#Handle',
  \})
" }}
