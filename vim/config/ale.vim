" Ale {{{
  " Better ALE Msg Format
  " let g:ale_echo_msg_error_str = 'E'
  " let g:ale_echo_msg_warning_str = 'W'
  let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
  "
  " Jump between ALE Errors:
  nmap <silent> <leader>lk <Plug>(ale_previous_wrap)
  nmap <silent> <leader>lj <Plug>(ale_next_wrap)
  nmap <silent> <leader>ld :ALEDetail<CR>
  "
  let g:ale_lint_on_insert_leave = 1
  let g:ale_cursor_detail = 0
  let g:ale_linters = {
    \  'javascript':  ['eslint', 'jshint', 'flow'],
    \  'go': ['govet', 'gofmt', 'golint'],
    \  'reason': ['reason-language-server'],
    \}
  " ALE's support for RLS is worthless:
  let g:ale_reason_ls_executable = '/usr/local/bin/reason-language-server'
  " Add Vale to pandoc bc vim-pandoc insists on changing filetype:
  call ale#linter#Define('pandoc', {
  \   'name': 'vale',
  \   'executable': 'vale',
  \   'command': 'vale --output=JSON %t',
  \   'callback': 'ale#handlers#vale#Handle',
  \})
" }}}
" # vim:foldmethod=marker

