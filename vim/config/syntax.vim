" Syntax:
" Tabstops {{{
  augroup my-tabstops
    autocmd!
    " Go
    autocmd FileType go setlocal tabstop=4
    autocmd FileType go setlocal shiftwidth=4
    autocmd FileType go setlocal noexpandtab
    " JavaScript
    autocmd FileType javascript,javascriptreact setlocal tabstop=2
    autocmd FileType javascript,javascriptreact setlocal shiftwidth=2
    autocmd FileType javascript,javascriptreact setlocal softtabstop=2
    autocmd FileType javascript,javascriptreact setlocal expandtab
    " Markdown
    autocmd FileType markdown,pandoc,md setlocal tabstop=4
    autocmd FileType markdown,pandoc,md setlocal shiftwidth=4
    autocmd FileType markdown,pandoc,md setlocal noexpandtab
  augroup END
" }}}
" Line numbers {{{
  set number relativenumber
  augroup line-numbers
    autocmd!
    if has('nvim')
      autocmd TermOpen * setlocal nonumber norelativenumber
    endif
  augroup END
" }}}
" Syntax Autocmds {{{
  " Proper keyword highlighting for CSS:
  augroup VimCSS3Syntax
    autocmd!
    autocmd FileType css setlocal iskeyword+=-
  augroup END
" }}}
" Plugins {{{
" auto-pairs {{{
  let g:AutoPairs = {'(':')', '[':']',"'":"'",'"':'"', '`':'`', '{': '}'}
  " ,'```':'```', '"""':'"""', "'''":"'''"})
  augroup autopair-enable
    autocmd!
    autocmd FileType markdown,pandoc
      \ let b:AutoPairs = extend(g:AutoPairs, {'~~~':'~~~'})
  augroup END
" }}}
" vim-rooter {{{
  " Turn off autoroot for non-project files:
  let g:rooter_patterns = ['Rakefile', 'package.json', '.git/', 'Gemfile']
  " let g:rooter_change_directory_for_non_project_files = '' " can be current or home
  " let g:rooter_use_lcd = 1 " only change the current window
" }}}
" vim-closetag {{{
  let g:closetag_filenames = '*.html,*.xhtml,*.phtml,*.php,*.js,*.erb'
  let g:closetag_xhtml_filenames = '*.xhtml,*.js,*.erb'
  let g:closetag_filetypes = 'html,markdown,javascriptreact'
  let g:closetag_close_shortcut = '<leader>>'
  let g:closetag_regions = {
    \ 'typescriptreact': 'jsxRegion,tsxRegion',
    \ 'javascriptreact': 'jsxRegion',
    \ 'javascript': 'jsxRegion',
    \ }
" }}}
" vim-json {{{
  let g:vim_json_syntax_conceal = 0
" }}}
" indentLine {{{
  let g:indentLine_setColors = 1
  let g:indentLine_char = "\u22EE"
  let g:indentLine_color_term = 11
" }}}
" }}}
" # vim:foldmethod=marker
