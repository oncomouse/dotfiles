" Syntax:
" General VIM {{{
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
" Emmet {{{
  let g:user_emmet_settings = {
    \  'javascript.jsx' : {
      \      'extends' : 'jsx',
      \  },
    \}
" }}}
" Coc JSONC {{{
  augroup jsonc-syntax-coc
    autocmd!
    autocmd FileType json syntax match Comment +\/\/.\+$+
  augroup END
" }}}
" Tabstops {{{
  augroup my-tabstops
    autocmd!
    " Go
    autocmd FileType go setlocal tabstop=4
    autocmd FileType go setlocal shiftwidth=4
    autocmd FileType go setlocal noexpandtab
    " JavaScript
    autocmd FileType javascript,javascript.jsx setlocal tabstop=2
    autocmd FileType javascript,javascript.jsx setlocal shiftwidth=2
    autocmd FileType javascript,javascript.jsx setlocal softtabstop=2
    autocmd FileType javascript,javascript.jsx setlocal expandtab
    " Markdown
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
    if has('nvim')
      autocmd TermOpen * setlocal nonumber norelativenumber
    endif
  augroup END
" }}}
" Closetag {{{
  let g:closetag_filenames = "*.html,*.xhtml,*.phtml,*.php,*.js,*.erb"
  let g:closetag_xhtml_filenames = '*.xhtml,*.js,*.erb'
  let g:closetag_close_shortcut = '<leader>>'
  let g:closetag_regions = {
    \ 'typescript.tsx': 'jsxRegion,tsxRegion',
    \ 'javascript.jsx': 'jsxRegion',
    \ 'javascript': 'jsxRegion',
    \ }
" }}}
" # vim:foldmethod=marker
