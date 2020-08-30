" Syntax:
" Tabstops {{{
  augroup my-tabstops
    autocmd!
    " Go, Lua
    autocmd FileType go,lua setlocal tabstop=4
    autocmd FileType go,lua setlocal shiftwidth=4
    autocmd FileType go,lua setlocal noexpandtab
    " JavaScript
    autocmd FileType javascript,javascriptreact setlocal tabstop=2
    autocmd FileType javascript,javascriptreact setlocal shiftwidth=2
    autocmd FileType javascript,javascriptreact setlocal softtabstop=2
    autocmd FileType javascript,javascriptreact setlocal expandtab
    " Markdown
    autocmd FileType markdown setlocal tabstop=4
    autocmd FileType markdown setlocal shiftwidth=4
    autocmd FileType markdown setlocal noexpandtab
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
      autocmd FileType markdown
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
  " vim-sandwich {{{
    let g:sandwich#recipes = deepcopy(g:sandwich#default_recipes)
    " sa<target>b adds Bold:
    let g:sandwich#recipes += [
          \{ 
          \ 'buns': ['**', '**'],
          \ 'filetype': ['markdown'],
          \ 'kind': ['add', 'replace'],
          \ 'input': ['b'],
          \},
          \]
    " sa<target>t adds HTML tags:
    let g:sandwich#recipes += [
          \   {
          \     'buns'    : ['TagInput(1)', 'TagInput(0)'],
          \     'expr'    : 1,
          \     'filetype': ['html'],
          \     'kind'    : ['add', 'replace'],
          \     'action'  : ['add'],
          \     'input'   : ['t'],
          \   },
          \ ]

    function! TagInput(is_head) abort
      if a:is_head
        let s:TagLast = input('Tag: ')
        if s:TagLast !=# ''
          let tag = printf('<%s>', s:TagLast)
        else
          throw 'OperatorSandwichCancel'
        endif
      else
        let tag = printf('</%s>', matchstr(s:TagLast, '^\a[^[:blank:]>/]*'))
      endif
      return tag
    endfunction
  " }}}
" }}}
" # vim:foldmethod=marker
