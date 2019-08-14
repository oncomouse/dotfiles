" Linting
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
  " Define the linters we plan to use and disable all others:
  let g:ale_linters_explicit = 1
  let g:ale_linters = {
    \  'css': ['stylelint'],
    \  'go': ['govet', 'gofmt', 'golint'],
    \  'javascript':  ['eslint'],
    \  'pandoc': ['vale'],
    \  'python': ['pylint', 'bandit'],
    \  'reason': [],
    \  'ruby': ['rubocop', 'ruby'],
    \  'scss': ['stylelint'],
    \  'vim': ['vint'],
    \  'yaml': ['yamllint'],
    \}
  let g:ale_pattern_options = {
    \  '\.min.js$': {'ale_enabled': 0},
    \  'build/.*$': {'ale_enabled': 0},
    \}
  " Add Vale to pandoc bc vim-pandoc insists on changing filetype:
  call ale#linter#Define('pandoc', {
  \   'name': 'vale',
  \   'executable': 'vale',
  \   'command': 'vale --output=JSON %t',
  \   'callback': 'ale#handlers#vale#Handle',
  \})
" }}}
" # vim:foldmethod=marker
