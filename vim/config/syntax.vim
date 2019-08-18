" Syntax:
" General VIM {{{
  " Insert a semicolon at the end of the line with <leader>;
  let g:python_highlight_all = 1
  " Proper keyword highlighting for CSS:
  augroup VimCSS3Syntax
    autocmd!

    autocmd FileType css setlocal iskeyword+=-
  augroup END
" }}}
" vim-jsx-pretty {{{
  let g:vim_jsx_pretty_colorful_config = 1
  let g:vim_json_syntax_conceal = 0
" }}}
" Line Indent {{{
  let g:indentLine_setColors = 1
  let g:indentLine_char = "\u22EE"
  let g:indentLine_color_term = 11
  " let g:indent_guides_enable_on_vim_startup = 1
  " let g:indent_guides_auto_colors = 0
  " augroup indent-colors
  "   autocmd!
  "   autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=#1B2B34 ctermbg=NONE
  "   autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=#1C313D ctermbg=0
  " augroup END
" }}}
" Rainbow Parentheses {{{
  let g:rainbow_active = 1
  let g:rainbow_conf = {
  \ 'guifgs': ['#C594C5', '#FAC863','#6699CC','#5FB3B3','#F99157','#AB7967','#99C794','#EC5f67'],
  \ 'ctermfgs': [5, 3, 4, 6, 9, 14, 2, 1],
  \ 'separately': {
  \   '*': {},
  \   'markdown': {
  \     'parentheses_options': 'containedin=markdownCode contained',
  \   },
  \   'css': 0,
  \   'scss': 0,
  \ }
  \}
" }}}
" Autoroot {{{
  " Turn off autoroot for non-project files:
  let g:rooter_patterns = ['project.clj', 'Rakefile', 'package.json', '.git/', 'go.mod', 'bsconfig.json']
  " let g:rooter_change_directory_for_non_project_files = '' " can be current or home
  " let g:rooter_use_lcd = 1 " only change the current window
" }}}
" Colorizer {{{
  let g:colorizer_auto_filetype='css,scss,sass,less,html,javascript,javascript.jsx,vim'
  let g:colorizer_disable_bufleave = 1
" }}}
" Emmet {{{
  let g:user_emmet_settings = {
    \  'javascript.jsx' : {
      \      'extends' : 'jsx',
      \  },
    \}
" }}}
" Better Whitespace {{{
  let g:better_whitespace_guicolor='#EC5f67'
  let g:strip_whitespace_on_save=1
" }}}
" CoC JSONC {{{
  augroup jsonc-syntax-coc
    autocmd!
    autocmd FileType json syntax match Comment +\/\/.\+$+
  augroup END
" }}}
" Tabstops {{{
augroup my-go-tabstops
    autocmd!
    autocmd FileType go setlocal tabstop=4
    autocmd FileType go setlocal shiftwidth=4
    autocmd FileType go setlocal noexpandtab
  augroup END
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
" }}}
" Autopair {{{
  let g:AutoPairs = {'(':')', '[':']',"'":"'",'"':'"', '`':'`', '{': '}'}
  " ,'```':'```', '"""':'"""', "'''":"'''"})
  augroup autopair-enable
    autocmd!
    autocmd FileType markdown,pandoc
      \ let b:AutoPairs = extend(g:AutoPairs, {'~~~':'~~~'})
  augroup END
" }}}
" Line numbers {{{
  augroup line-numbers
    set number relativenumber
    autocmd!
    " autocmd BufEnter,WinEnter * if &filetype=='pandoc' || &filetype=='text'|setlocal nonumber norelativenumber|endif
    if has('nvim')
      autocmd TermOpen * setlocal nonumber norelativenumber
    endif
  augroup END
" }}}
" # vim:foldmethod=marker
