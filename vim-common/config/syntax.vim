" Syntax:
" CoC {{
  vmap <S-f> <Plug>(coc-format-selected)<CR>
  nmap <leader>f <Plug>(coc-format-selected)
  command! -nargs=0 Format :call CocAction('format')
" }}
" Ale {{
  " Better ALE Msg Format
  " let g:ale_echo_msg_error_str = 'E'
  " let g:ale_echo_msg_warning_str = 'W'
  let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
  "
  " Jump between ALE Errors:
  nmap <silent> <C-k> <Plug>(ale_previous_wrap)
  nmap <silent> <C-j> <Plug>(ale_next_wrap)
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
" vim-jsx-pretty {{
  let g:vim_jsx_pretty_colorful_config = 1
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
  let g:rainbow_active = 1
  let g:rainbow_conf = {
  \	'guifgs': ['firebrick', 'purple', 'seagreen3', 'royalblue3', 'darkorange3', 'darkcyan'],
  \ 'separately': {
  \   '*': {},
  \   'markdown': {
  \     'parentheses_options': 'containedin=markdownCode contained',
  \   }
  \ }
  \}
" }}
" Autoroot {{
  " Turn off autoroot for non-project files:
  let g:rooter_patterns = ['project.clj', 'Rakefile', 'package.json', '.git/']
  " let g:rooter_change_directory_for_non_project_files = '' " can be current or home
  " let g:rooter_use_lcd = 1 " only change the current window
" }}
" Colorizer {{
  let g:colorizer_auto_filetype='css,scss,sass,less,html,javascript,javascript.jsx'
" }}
" Emmet
  let g:user_emmet_settings = {
    \  'javascript.jsx' : {
      \      'extends' : 'jsx',
      \  },
    \}
" }}
" Better Whitespace {{
  let g:better_whitespace_guicolor='#EC5f67'
  let g:strip_whitespace_on_save=1
" }}
" CoC JSONC {{
  autocmd FileType json syntax match Comment +\/\/.\+$+
" }}
" vim-clojure-static {{
  " Use vim-clojure-static for the omnifunc:
  augroup clojure-autocomplete
    autocmd!
    autocmd BufEnter,WinEnter * if &filetype=='clojure'|setlocal omnifunc=clojurecomplete#Complete|endif
  augroup END
" }}
