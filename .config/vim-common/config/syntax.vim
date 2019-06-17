" Syntax:
" Make sure .md files are read as Markdown:
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
" Load Indent Guides for Python Files:
autocmd BufNewFile,BufReadPost *.py IndentGuidesEnable

" Use vim-jsx-improved instead:
let g:polyglot_disabled = ['jsx']

" Start with indent guides on:
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_auto_colors = 0
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=#182933   ctermbg=235
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=#1c313d ctermbg=234

" Turn off autoroot for non-project files:
let g:rooter_patterns = ['project.clj', 'Rakefile', 'package.json', '.git/']

" let g:rooter_change_directory_for_non_project_files = '' " can be current or home
" let g:rooter_use_lcd = 1 " only change the current window
