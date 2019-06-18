" Syntax:
" Make sure .md files are read as Markdown:
" augroup markdown
"   autocmd!
"   autocmd BufNewFile,BufReadPost *.md set filetype=markdown
" augroup END
call ale#linter#Define('pandoc', {
\   'name': 'vale',
\   'executable': 'vale',
\   'command': 'vale --output=JSON %t',
\   'callback': 'ale#handlers#vale#Handle',
\})

" Use vim-jsx-improved instead:
let g:polyglot_disabled = ['jsx']

" Start with indent guides on:
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_auto_colors = 0
augroup indent-colors
  autocmd!

  autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=#1B2B34 ctermbg=235
  autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=#1C313D ctermbg=234
augroup END

" Setup rainbow-parentheses:
augroup rainbow-parentheses
  autocmd!
  autocmd VimEnter * RainbowParenthesesToggle
  autocmd Syntax * RainbowParenthesesLoadRound
  autocmd Syntax * RainbowParenthesesLoadSquare
  autocmd Syntax * RainbowParenthesesLoadBraces
augroup END

" Turn off autoroot for non-project files:
let g:rooter_patterns = ['project.clj', 'Rakefile', 'package.json', '.git/']
" let g:rooter_change_directory_for_non_project_files = '' " can be current or home
" let g:rooter_use_lcd = 1 " only change the current window
