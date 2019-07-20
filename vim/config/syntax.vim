" Syntax:
" vim-jsx-pretty {{
  let g:vim_jsx_pretty_colorful_config = 1
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
  \ 'guifgs': ['#F99157', '#EC5f67', '#C594C5', '#5FB3B3', '#6699CC', '#FAC863', '#99C794', '#AB7967'],
  \ 'ctermfgs': [5, 4, 6, 2, 1],
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
  let g:colorizer_auto_filetype='css,scss,sass,less,html,javascript,javascript.jsx,vim'
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
  augroup jsonc-syntax-coc
    autocmd!
    autocmd FileType json syntax match Comment +\/\/.\+$+
  augroup END
" }}
" vim-clojure-static {{
  " Use vim-clojure-static for the omnifunc:
  augroup clojure-autocomplete
    autocmd!
    autocmd BufEnter,WinEnter * if &filetype=='clojure'|setlocal omnifunc=clojurecomplete#Complete|endif
  augroup END
" }}
" vim-go {{
  let g:go_fmt_command = "goimports"
  let g:go_highlight_build_constraints = 1
  let g:go_highlight_extra_types = 1
  let g:go_highlight_fields = 1
  let g:go_highlight_functions = 1
  let g:go_highlight_methods = 1
  let g:go_highlight_operators = 1
  let g:go_highlight_structs = 1
  let g:go_highlight_types = 1
  let g:go_auto_type_info = 1
  augroup my-go-tabstops
    autocmd!
    autocmd FileType go setlocal tabstop=4
    autocmd FileType go setlocal shiftwidth=4
    autocmd FileType go setlocal noexpandtab

    " Turn off omnifunc in go:
    " autocmd BufEnter,WinEnter * if &filetype=='go'|setlocal omnifunc=|endif
    " Bind goto function definitions:
    autocmd FileType go nmap <leader>gd :GoDeclsDir<cr>
    au FileType go nmap <F12> <Plug>(go-def)
  augroup END
" }}
" Other tabstops {{
  augroup my-js-tabstops
    autocmd!
    autocmd FileType javascript,javascript.jsx setlocal tabstop=2
    autocmd FileType javascript,javascript.jsx setlocal shiftwidth=2
    autocmd FileType javascript,javascript.jsx setlocal softtabstop=2
    autocmd FileType javascript,javascript.jsx setlocal expandtab
  augroup END
  augroup my-md-tabstops
    autocmd!
    autocmd FileType markdown,pandoc,md setlocal tabstop=4
    autocmd FileType markdown,pandoc,md setlocal shiftwidth=4
    autocmd FileType markdown,pandoc,md setlocal noexpandtab
  augroup END
" }}
" Sexp Mappings {{
" let g:sexp_filetypes = 'lisp,scheme,clojure,javascript'
" }}
" Autopair {{
  let g:AutoPairs = {'(':')', '[':']',"'":"'",'"':'"', "`":"`"}
  " ,'```':'```', '"""':'"""', "'''":"'''"})
  augroup autopair-enable
    autocmd!
    autocmd FileType javascript,javascript.jsx,vim,clojure,go
      \ let b:AutoPairs = extend(g:AutoPairs, { '{':'}' })
    autocmd FileType markdown,pandoc
      \ let b:AutoPairs = extend(g:AutoPairs, {'~~~':'~~~'})
  augroup END
" }}
