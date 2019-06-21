" Syntax:
" Ale {{
  " Better ALE Msg Format
  " let g:ale_echo_msg_error_str = 'E'
  " let g:ale_echo_msg_warning_str = 'W'
  let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
  let g:lightline#ale#indicator_checking = "\uf110"
  let g:lightline#ale#indicator_warnings = "\uf071\u2003"
  let g:lightline#ale#indicator_errors = "\uf05e\u2003"
  let g:lightline#ale#indicator_ok = "\uf00c"
  "
  " Jump between ALE Errors:
  nmap <silent> <C-k> <Plug>(ale_previous_wrap)
  nmap <silent> <C-j> <Plug>(ale_next_wrap)
  "
  " Lint only on save:
  " let g:ale_lint_on_text_changed = 'never'
  " let g:ale_lint_on_enter = 1
  "
  " Add Vale to pandoc bc vim-pandoc insists on changing filetype:
  call ale#linter#Define('pandoc', {
  \   'name': 'vale',
  \   'executable': 'vale',
  \   'command': 'vale --output=JSON %t',
  \   'callback': 'ale#handlers#vale#Handle',
  \})
" }}
" Polyglot {{
  let g:polyglot_disabled = ['jsx', 'pandoc', 'javascript'] " Disable JSX, JavaScript, and Pandoc syntax:
" }}
" Line Indent Colors {{
  let g:indent_guides_enable_on_vim_startup = 1
  let g:indent_guides_auto_colors = 0
  augroup indent-colors
    autocmd!
    autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=#1B2B34 ctermbg=235
    autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=#1C313D ctermbg=234
  augroup END
" }}
" Rainbow Parentheses {{
  augroup rainbow-parentheses
    autocmd!
    autocmd VimEnter * RainbowParenthesesToggle
    autocmd Syntax * RainbowParenthesesLoadRound
    autocmd Syntax * RainbowParenthesesLoadSquare
    autocmd Syntax * RainbowParenthesesLoadBraces
  augroup END
" }}
" Autoroot {{
  " Turn off autoroot for non-project files:
  let g:rooter_patterns = ['project.clj', 'Rakefile', 'package.json', '.git/']
  " let g:rooter_change_directory_for_non_project_files = '' " can be current or home
  " let g:rooter_use_lcd = 1 " only change the current window
" }}
" Connect Deoplete to clojure-vim/async-clj-omni {{
  call deoplete#custom#option('keyword_patterns', {
    \ 'clojure': '[\w!$%&*+/:<=>?@\^_~\-\.#]*'
  \})
" }}
" Colorizer {{
  let g:colorizer_auto_filetype='css,scss,sass,less,html'
" }}
" Emmet
  let g:user_emmet_settings = {
    \  'javascript.jsx' : {
      \      'extends' : 'jsx',
      \  },
    \}
" }}
